(ns indra-clj.mobius-test
  (:require [indra-clj.complex :as c]
            [indra-clj.mobius :as m]
            [clojure.test :refer [deftest is testing]]))

(deftest fixed-points-test
  (testing "textbook example 1"
    (let [k (c/rect 1 0.4)
          spiral-map (m/->Transformation k c/zero c/zero c/one)
          rs-rotation-map (m/->Transformation c/one (c/- c/one) c/one c/one)
          conj-map (m/conjugate spiral-map rs-rotation-map)
          scaled-conj-map (m/scale conj-map (c/reciprocal (c/rect 0 0.4)))
          normalized-conj-map (m/make-transformation scaled-conj-map)
          [fp1 fp2] (->> (m/fixed-points normalized-conj-map)
                         (sort-by #(-> % (c/- c/one) c/abs)))]
      (is (= (m/->Transformation (c/rect 1 -5) c/one c/one (c/rect 1 -5))
             scaled-conj-map))
      (is (c/=* fp1 (c/rect 1 0)))
      (is (c/=* fp2 (c/rect -1 0))))))
