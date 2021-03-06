(ns indra.mobius-test
  (:require [indra.complex :as c]
            [indra.mobius :as m]
            [clojure.test :refer [deftest is testing]]))

(deftest map-points-test
  (testing "T(∞)"
    (let [; some arbitary transform
          a (c/rect 1 2)
          b (c/rect 3 4)
          c (c/rect 5 6)
          d (c/rect 7 8)
          t (m/make-transformation a b c d)]
      (is (c/=* (c/div a c)
                (m/transform c/inf t))))))

(deftest fixed-points-test
  (testing "textbook example 1"
    (let [k (c/rect 1 0.4)
          spiral-map (m/->Transformation k c/zero c/zero c/one)
          rs-rotation-map (m/->Transformation c/one (c/- c/one) c/one c/one)
          conj-map (m/conjugate spiral-map rs-rotation-map)
          scaled-conj-map (m/scale conj-map (c/reciprocal (c/rect 0 0.4)))
          normalized-conj-map (m/make-transformation scaled-conj-map)
          [sink source] (->> (m/fixed-points normalized-conj-map)
                             (sort-by #(-> % (c/- c/one) c/abs)))]
      (is (= (m/->Transformation (c/rect 1 -5) c/one c/one (c/rect 1 -5))
             scaled-conj-map))
      (is (c/=* sink (c/rect 1 0)))
      (is (c/=* source (c/rect -1 0))))))
