(ns indra.schottky
  (:require [indra.mobius :as m]
            [indra.mobius.recipes :as r])
  (:import [org.apache.commons.math3.util FastMath]))

(defn pairing-transforms
  [[c c* t] & rest]
  (let [s (r/pair-circles c c* (or t m/unit))]
    (concat
     [s (m/inverse s)]
     (when (not-empty rest)
       (apply pairing-transforms rest)))))

(defn word-list
  [max-depth]
  (loop [depth 1
         frozen []
         previous [[:a] [:b] [:A] [:B]]]
    (if (= depth max-depth)
      (into frozen previous)
      (recur (inc depth)
             (into frozen previous)
             (for [word previous
                   letter [:a :b :A :B]
                   :when (not= letter
                               (case (peek word)
                                 :a :A, :A :a, :b :B, :B :b))]
               (conj word letter))))))

(defn schottkey-disks
  ([a1 a2 b1 b2 depth] (schottkey-disks a1 a2 b1 b2 depth nil nil))
  ([a1 a2 b1 b2 depth ta tb]
   (let [[a1->a2 a2->a1 b1->b2 b2->b1] (pairing-transforms [a1 a2 ta] [b1 b2 tb])]
     (schottkey-disks a1 a2 b1 b2 depth {:a a2->a1 :A a1->a2 :b b2->b1 :B b1->b2})))
  ([a1 a2 b1 b2 depth transforms]
   (let [init-disks {:a a1 :A a2 :b b1 :B b2}]
     ;; technically, we have the letter order backwards here but it doesnt matter so whatevs
     (for [[head & tail :as word] (word-list depth)]
       {:depth (count word)
        :disk (reduce (fn [acc x]
                        (m/transform acc (get transforms x)))
                      (get init-disks head)
                      tail)
        :word word}))))


(comment
  (word-list 2)

  (require '[indra.complex :as c]
           '[indra.geometry :as g])

  (schottkey-disks (g/->Circle c/one 0.5)
                   (g/->Circle (c/- c/one) 0.5)
                   (g/->Circle c/i 0.4)
                   (g/->Circle (c/- c/i) 0.4)
                   2)
  )
