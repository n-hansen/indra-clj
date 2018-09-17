(ns indra.complex-test
  (:require [indra.complex :as c]
            [clojure.test :refer [deftest is]]))

;; TODO property based testing

(deftest infinity-arithmetic
  (is (c/inf? (c/reciprocal c/zero)))
  (is (c/zero? (c/reciprocal c/inf)))
  (is (c/inf? (c/+ c/one c/inf)))
  (is (c/inf? (c/- c/one c/inf)))
  (is (c/inf? (c/* c/one c/inf)))
  (is (c/inf? (c/div c/inf c/one)))
  (is (c/zero? (c/div c/one c/inf)))
  (is (c/inf? (c/div c/one c/zero))))
