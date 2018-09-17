(ns indra-clj.complex-test
  (:require [indra-clj.complex :as c]
            [clojure.test :refer [deftest is]]))

;; TODO property based testing

(deftest infinity-arithmetic
  (is (c/inf? (c/reciprocol c/zero)))
  (is (c/zero? (c/reciprocol c/inf)))
  (is (c/inf? (c/+ c/one c/inf)))
  (is (c/inf? (c/- c/one c/inf)))
  (is (c/inf? (c/* c/one c/inf)))
  (is (c/inf? (c/div c/inf c/one)))
  (is (c/zero? (c/div c/one c/inf)))
  (is (c/inf? (c/div c/one c/zero))))
