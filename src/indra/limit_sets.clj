(ns indra.limit-sets
  (:require [indra.mobius :as m]
            [indra.complex :as c]))

;; DFS with static depth and fixed preimage list

(defn word->transform
  [a a* b b* word]
  (->> word
       (map (fn [ltr]
              (case ltr
                :a a
                :A a*
                :b b
                :B b*)))
       (reduce m/compose)))

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
                          ;; TODO special repetends
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

(defn limit-set-dfs
  ([a b max-depth epsilon] (limit-set-dfs a b max-depth epsilon nil))
  ([a b max-depth epsilon special-repetends]
   (for [branch [:a :b :A :B]
         point (limit-set-dfs-section a b max-depth epsilon special-repetends [branch (first-child branch)] nil)]
     point)))
