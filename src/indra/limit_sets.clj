(ns indra.limit-sets
  (:require [indra.mobius :as m]))

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

(defn prev-letter
  [ltr]
  (case ltr
    :b :a
    :A :b
    :B :A
    :a :B))

(defn first-word
  "first word of length n on the cayley graph"
  [depth]
  (loop [w [:a]]
    (if (= depth (count w))
      w
      (recur (conj w (-> w peek prev-letter))))))

(defn next-word
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
                              (-> word (nth ix) (prev-letter)))
                       (inc ix))))))))))

(defn all-words
  "all words of length n"
  [n]
  (->> (first-word n)
       (iterate next-word)
       (take-while some?)))

(defn limit-set-fixed-depth-dfs
  [a a* b b* depth preimages]
  (for [word (all-words depth)
        p preimages]
    (m/transform p (word->transform a a* b b* word))))
