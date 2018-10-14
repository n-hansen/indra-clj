(ns indra.limit-sets-test
  (:require [clojure.test :as t :refer [deftest testing is]]
            [indra.complex :as c]
            [indra.limit-sets :refer :all]
            [indra.mobius :as m]))

(deftest letter-test
  (testing "next-letter is cyclic"
    (let [letters [:a :b :A :B]]
      (is (every? #(-> % next-letter next-letter next-letter next-letter (= %))
                  letters))))
  (testing "letters are in the correct cyclic order"
    (let [letters [:a :b :A :B]]
      (is (every? #(-> % next-letter next-letter (inverse-letter? %))
                  letters)))))

(deftest next-word-test
  (testing "increment"
    (is (= (next-word [:a]) [:b]))
    (is (= (next-word [:b]) [:A]))
    (is (= (next-word [:A]) [:B]))
    (is (= (next-word [:B]) nil))
    (is (= (next-word [:a :a]) [:a :b])))
  (testing "carry"
    (is (= (next-word [:a :b]) [:b :a]))
    (is (= (next-word [:a :b :A]) [:b :a :B]))
    (is (= (next-word [:B :a]) nil)))
  (testing "a long sequence"
    (let [words (all-words 10)]
      (is (= (apply * 4 (repeat 9 3))
             (count words)
             (count (set words))))
      (is (every? #(= 10 (count %)) words)))))
