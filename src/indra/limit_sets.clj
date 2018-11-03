(ns indra.limit-sets
  (:require [indra.mobius :as m]
            [indra.complex :as c])
  (:import [java.util ArrayDeque]))

;; DFS with static depth and fixed preimage list

(defn word->transform
  ([a b word] (word->transform a (m/inverse a) b (m/inverse b) word))
  ([a a* b b* word]
   (->> word
        (map {:a a :A a* :b b :B b*})
        (reduce m/compose))))

(defn inverse-letter?
  [ltr1 ltr2]
  (case ltr1
    :a (= :A ltr2)
    :A (= :a ltr2)
    :b (= :B ltr2)
    :B (= :b ltr2)))

(defn next-letter
  [ltr]
  (case ltr
    :a :b
    :b :A
    :A :B
    :B :a))

(defn previous-letter
  [ltr]
  (case ltr
    :b :a
    :A :b
    :B :A
    :a :B))

; some useful synonymns
(def first-child previous-letter)
(def last-child next-letter)

(defn first-word-at-depth
  "first word of length n on the cayley graph"
  [depth]
  (loop [w [:a]]
    (if (= depth (count w))
      w
      (recur (conj w (-> w peek first-child))))))

(defn next-word-at-depth
  "traversal of the cayley graph keeping depth constant"
  [word]
  (let [final-ix (dec (count word))]
    (loop [word word
           ix final-ix]
      (when-not (neg? ix)
        (let [x (next-letter (nth word ix))]
          (if (inverse-letter? x (nth word (dec ix) :A)) ; root node order is :a :b :A :B
            ;; walk towards the cayley graph root
            (recur word (dec ix))
            ;; orient towards next child then descend
            (loop [word (assoc word ix x)
                   ix ix]
              (if (= ix final-ix)
                word
                (recur (assoc word
                              (inc ix)
                              (-> word (nth ix) (first-child)))
                       (inc ix))))))))))

(defn all-words
  "all words of length n in dfs order"
  [n]
  (->> (first-word-at-depth n)
       (iterate next-word-at-depth)
       (take-while some?)))

(defn limit-set-fixed-depth-dfs
  [a a* b b* repetends depth]
  (let [preimages (for [repetend repetends]
                    (->> repetend
                         (word->transform a a* b b*)
                         m/fixed-points
                         first))]
    (for [word (all-words depth)
          p preimages]
      (m/transform p (word->transform a a* b b* word)))))

;; DFS for limit points to a maximum depth or displacement

(defn cyclic-permutations
  [word]
  (->> (concat word word)
       (iterate rest)
       (take (count word))
       (map #(->> % (take (count word)) vec))))

(defn next-word-and-ascend
  "increment the least-significant letter of the word, ascending the cayley graph as necessary.

  bottoms out at [x (last-child x)] (i.e. you won't cycle between top level branches) ¯|_(ツ)_|¯"
  [word]
  (let [this (peek word)
        ancestors (pop word)]
    (when-let [parent (peek ancestors)]
      (if (not= this (last-child parent))
        (conj ancestors (next-letter this))
        (recur ancestors)))))

(defn repetend-table
  "generate a set of repetends to examine for a given prefix.

  will return the initial and final cyclic permutations of the commutator, plus
  any special repetends whose final letter matches the final letter of the prefix."
  [special-repetends]
  (into {} (for [ltr [:a :b :A :B]
                 :let [initial-commutator (->> (iterate first-child ltr)
                                               (drop 1)
                                               (take 4)
                                               (into []))
                       final-commutator (->> (iterate last-child ltr)
                                             (drop 1)
                                             (take 4)
                                             (into []))]]
             [ltr (concat [initial-commutator]
                          ; TODO handle the case where we have multiple special repetends per entry
                          (filter #(= ltr (peek %)) special-repetends)
                          [final-commutator])])))

(defn limit-set-dfs-section
  [a b max-depth epsilon special-repetends start end]
  (let [a* (m/inverse a)
        b* (m/inverse b)
        preimages (->> (repetend-table special-repetends)
                       (map (fn [[ltr reps]]
                              [ltr (mapv #(->> %
                                               (word->transform a a* b b*)
                                               m/fixed-points
                                               first)
                                         reps)]))
                       (into {}))
        go (fn continue [current-word]
             (when (and (some? current-word)
                        (not= current-word end))
               (let [t (word->transform a a* b b* current-word)
                     test-points (->> (peek current-word)
                                      (preimages)
                                      (map #(m/transform % t)))]
                 (if (or (= max-depth (count current-word))
                         (->> test-points
                              (partition 2 1)
                              (every? (fn [[z1 z2]]
                                        (> epsilon
                                           (c/abs (c/- z1 z2)))))))
                   (lazy-cat test-points
                             (continue (next-word-and-ascend current-word)))
                   (recur (conj current-word
                                (-> current-word peek first-child)))))))]
    (go start)))

;; caching dfs implementation

; ArrayDeque performs better than using volatiles and/or transients and is easier to understand to boot.
; NB:  Adding pmaps will cause a spooky ghost to visit your
;      house at night and make a mess of your pots and pans.
(defrecord DfsContext [generators ^ArrayDeque letters ^ArrayDeque transformations])

(defn create-dfs-context
  [a b]
  (->DfsContext {:a a :A (m/inverse a) :b b :B (m/inverse b)}
                (ArrayDeque.)
                (doto (ArrayDeque.)
                  (.push m/unit))))

(defn pop-letter!
  [{:keys [^ArrayDeque transformations ^ArrayDeque letters] :as ctx}]
  (.removeLast transformations)
  (.removeLast letters))

(defn push-letter!
  [{:keys [generators ^ArrayDeque transformations ^ArrayDeque letters] :as ctx} letter]
  (.addLast letters letter)
  (.addLast transformations (m/compose (.peekLast transformations)
                                       (get generators letter))))

(defn current-depth
  [{:keys [^ArrayDeque letters] :as ctx}]
  (.size letters))

(defn current-letter
  [{:keys [^ArrayDeque letters] :as ctx}]
  (.peekLast letters))

(defn current-word
  [{:keys [^ArrayDeque letters] :as ctx}]
  (vec (.toArray letters)))

(defn current-transformation
  [{:keys [^ArrayDeque transformations] :as ctx}]
  (.peekLast transformations))

(defn advance-word!
  [{:keys [^ArrayDeque letters] :as ctx}]
  (let [prev-letter (pop-letter! ctx)]
    (when-let [parent-letter (.peekLast letters)]
      (if (= prev-letter (last-child parent-letter))
        (recur ctx)
        (push-letter! ctx (next-letter prev-letter))))))

(defn limit-set-caching-dfs-section
  [a b max-depth epsilon special-repetends start end]
  (let [ctx (create-dfs-context a b)
        preimages (->> (repetend-table special-repetends)
                       (map (fn [[ltr reps]]
                              [ltr (mapv #(-> (word->transform a b %)
                                              m/fixed-points
                                              first)
                                         reps)]))
                       (into {}))
        go (fn continue []
             (when-let [ltr (and (not (and end (= end (current-word ctx))))
                                 (current-letter ctx))]
               (let [t (current-transformation ctx)
                     test-points (map #(m/transform % t)
                                      (get preimages ltr))]
                 (if-not (and (> max-depth (current-depth ctx))
                              ; doing this dumb loop instead of (->> (partition) (every?))
                              ; improves our overall runtime by almost 50%
                              (loop [[z1 & zs] test-points]
                                (when-some [z2 (first zs)]
                                  (if (< epsilon
                                         (c/abs (c/- z1 z2)))
                                    true
                                    (recur zs)))))
                   (lazy-cat test-points
                             (do (advance-word! ctx)
                                 (continue)))
                   (do (push-letter! ctx (first-child ltr))
                       (recur))))))]
    (doseq [ltr start]
      (push-letter! ctx ltr))
    (go)))


(defn limit-set-dfs
  ([a b max-depth epsilon] (limit-set-dfs a b max-depth epsilon nil))
  ([a b max-depth epsilon special-repetends]
   (for [branch [:a :b :A :B]
         point (limit-set-caching-dfs-section a b max-depth epsilon special-repetends
                                              [branch (first-child branch)] nil)]
     point)))


(comment
  (require 'indra.mobius.recipes)
  (require 'criterium.core)

  (let [depth 400
        epsilon 5e-2
        {:keys [a b]} (indra.mobius.recipes/parabolic-commutator-group (c/rect 1.87 0.1)
                                                                       (c/rect 1.87 -0.1))]
    (criterium.core/quick-bench
     (count (limit-set-dfs-section a b depth epsilon
                                     [[:a] [:b] [:A] [:B]]
                                     [:a (first-child :a)]
                                     nil)))
    (time (->> (limit-set-dfs-section a b depth epsilon
                                      [[:a] [:b] [:A] [:B]]
                                      [:a (first-child :a)]
                                      nil)
               (count)
               (println "Generated points:"))))

  (let [depth 400
        epsilon 5e-2
        {:keys [a b]} (indra.mobius.recipes/parabolic-commutator-group (c/rect 1.87 0.1)
                                                                       (c/rect 1.87 -0.1))]
    (criterium.core/quick-bench
     (count (limit-set-caching-dfs-section a b depth epsilon
                                           [[:a] [:b] [:A] [:B]]
                                           [:a (first-child :a)]
                                           nil #_[:a (last-child :a)])))
    (time (->> (limit-set-caching-dfs-section a b depth epsilon
                                              [[:a] [:b] [:A] [:B]]
                                              [:a (first-child :a)]
                                              nil #_[:a (last-child :a)])
               (count)
               (println "Generated points:"))))

  )
