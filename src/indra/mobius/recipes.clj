(ns indra.mobius.recipes
  (:require [indra.complex :as c]
            [indra.geometry :as g]
            [indra.mobius :as m])
  (:import [org.apache.commons.math3.util FastMath]))

;; generating and checking properties of mobius transforms with some nice pre-determined recipes

(def inversion
  "inversion around the unit circle"
  (m/->Transformation c/zero c/one c/one c/zero))

(defn pure-translation
  "z ↦ z + b"
  [b]
  (m/->Transformation c/one b c/zero c/one))

(defn pure-scaling
  "z ↦ kz"
  [k]
  (let [sqrt-k (c/sqrt k)]
    (m/->Transformation sqrt-k c/zero c/zero (c/reciprocal sqrt-k))))

(defn pure-rotation
  [angle]
  (pure-scaling (c/polar 1.0 angle)))

(defn real-line-mapped-to-itself?
  "does the extended real line map to itself?"
  [t]
  (every? #(-> t % c/imag (= 0.0)) [:a :b :c :d]))

(def cayley-map
  "map the real axis to the unit circle"
  (m/->Transformation c/one (c/- c/i) c/one c/i))

(defn unit-circle-group
  "generate an element from the unit circle group, i.e. a map which carries the unit circle to itself.

  u and v are parameters which must be chosen s.t. |u|^2-|v|^2=1."
  [u v]
  (assert (= 1.0 (- (FastMath/pow (c/abs u) 2)
                    (FastMath/pow (c/abs v) 2))))
  (m/->Transformation u v (c/conjugate v) (c/conjugate u)))

(comment
  ; alternate definition for unit-circle-group
  (defn unit-circle-group
    [t]
    (assert (every? #(-> t % c/real?) [:a :b :c :d])
            "t must be a member of SL(2,R)")
    (m/conjugate cayley-map t)))

(defn special-stretch-map
  "generate a map which stretches the unit disk symmetrically away from a source at -1 to a sink at 1.

  this map is hyperbolic and a member of SL(2,R) (i.e. the upper half plane group).

  we require u > 1."
  [^double u]
  (assert (> u 1))
  (let [v (c/rect (FastMath/sqrt (- (* u u) 1.0)))
        u (c/rect u 0)]
    (m/make-transformation u v v u)))

(defn pair-circles
  "given c1 and c2, returns a pairing transformation.

  some member t of the unit circle group may also be provided to be performed before inversion"
  ([c1 c2] (pair-circles c1 c2 m/unit))
  ([c1 c2 t]
   (let [{p :center r :radius} c1
         {q :center s :radius} c2]
     (m/compose (pure-translation q)
                (pure-scaling (c/rect s 0))
                inversion
                t
                (pure-scaling (c/rect (/ r) 0))
                (pure-translation (c/- p))))))
