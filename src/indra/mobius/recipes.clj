(ns indra.mobius.recipes
  (:require [indra.complex :as c]
            [indra.geometry :as g]
            [indra.mobius :as m]))

;; generating and checking properties of mobius transforms with some nice pre-determined recipes

(defn pure-translation
  "z ↦ z + b"
  [b]
  (m/->Transformation c/one b c/one c/zero))

(defn pure-scaling
  "z ↦ kz"
  [k]
  (let [sqrt-k (c/sqrt k)]
    (m/->Transformation sqrt-k c/zero c/zero (c/reciprocal sqrt-k))))

(defn pure-rotation
  [angle]
  (m/->Transformation (c/polar 1.0 angle) c/zero c/zero c/one))

(defn real-line-mapped-to-itself?
  "does the extended real line map to itself?"
  [t]
  (every? #(-> t % c/imag (= 0)) [:a :b :c :d]))

(def cayley-map
  "map the real axis to the unit circle"
  (m/->Transformation c/one (c/- c/i) c/one c/i))
