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

(deftest limit-set-fixed-depth-dfs-fuchsian-test
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

(deftest next-word-and-ascend-test
  (testing "increment"
    (is (= (next-word-and-ascend [:a :a]) [:a :b]))
    (is (= (next-word-and-ascend [:b :b]) [:b :A]))
    (is (= (next-word-and-ascend [:A :A]) [:A :B]))
    (is (= (next-word-and-ascend [:B :B]) [:B :a])))
  (testing "ascend"
    (is (= (next-word-and-ascend [:a :a :b]) [:a :b]))
    (is (= (next-word-and-ascend [:a :b]) nil)))
  (testing "a long sequence"
    (is (= (->> (first-word-at-depth 3)
                (iterate next-word-and-ascend)
                (take-while some?))
           [[:a :B :A]
            [:a :B :B]
            [:a :B :a]
            [:a :a]
            [:a :b]]))))

(deftest repetend-table-test
  (testing "commutators"
    (is (= {:a [[:B :A :b :a]
                [:b :A :B :a]]
            :b [[:a :B :A :b]
                [:A :B :a :b]]
            :A [[:b :a :B :A]
                [:B :a :b :A]]
            :B [[:A :b :a :B]
                [:a :b :A :B]]}
           (repetend-table nil)))))

(deftest limit-set-dfs-fuchsian-test
  (let [sqrt2 (c/rect (FastMath/sqrt 2) 0)
        a (m/make-transformation sqrt2     c/i
                                 (c/- c/i) sqrt2)
        b (m/make-transformation sqrt2 c/one
                                 c/one sqrt2)
        distances  (->> (limit-set-dfs a b 12 1e-7 (first-word-at-depth 4) nil)
                        (map c/abs))
        ; can't crank precision past 1e-7 because that's what the fixed point of [:a :b :A :B] resolves to
        failures (remove #(Precision/equals 1.0 % 1e-7) distances)]
    (is (= 0 (count failures)))))
