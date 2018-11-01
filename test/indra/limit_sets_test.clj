(ns indra.limit-sets-test
  (:require [clojure.test :as t :refer [deftest testing is]]
            [indra.complex :as c]
            [indra.limit-sets :refer :all]
            [indra.mobius :as m])
  (:import [org.apache.commons.math3.util FastMath]
           [org.apache.commons.math3.util Precision]))

(deftest letter-test
  (testing "next-letter is cyclic"
    (let [letters [:a :b :A :B]]
      (is (every? #(-> % next-letter next-letter next-letter next-letter (= %))
                  letters))))
  (testing "letters are in the correct cyclic order"
    (let [letters [:a :b :A :B]]
      (is (every? #(-> % next-letter next-letter (inverse-letter? %))
                  letters)))))

(deftest next-word-at-depth-test
  (testing "increment"
    (is (= (next-word-at-depth [:a]) [:b]))
    (is (= (next-word-at-depth [:b]) [:A]))
    (is (= (next-word-at-depth [:A]) [:B]))
    (is (= (next-word-at-depth [:B]) nil))
    (is (= (next-word-at-depth [:a :a]) [:a :b])))
  (testing "carry"
    (is (= (next-word-at-depth [:a :b]) [:b :a]))
    (is (= (next-word-at-depth [:a :b :A]) [:b :a :B]))
    (is (= (next-word-at-depth [:B :a]) nil)))
  (testing "a long sequence"
    (let [words (all-words 10)]
      (is (= (apply * 4 (repeat 9 3))
             (count words)
             (count (set words))))
      (is (every? #(= 10 (count %)) words)))))

(deftest fuchsian-test
  (testing "limit set of a fuchsian group lies on the unit circle"
    (let [depth 10
          sqrt2 (c/rect (FastMath/sqrt 2) 0)
          a (m/make-transformation sqrt2     c/i
                                   (c/- c/i) sqrt2)
          a* (m/inverse a)
          b (m/make-transformation sqrt2 c/one
                                   c/one sqrt2)
          b* (m/inverse b)
          repetends  [[:a] [:b] [:A] [:B]]
          limit-set (into [] (limit-set-fixed-depth-dfs a a* b b* repetends depth))]
      (is (every? #(Precision/equals 1.0 (c/abs %) 1e-15) limit-set)))))
