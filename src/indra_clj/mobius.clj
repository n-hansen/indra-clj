(ns indra-clj.mobius
  (:require [indra-clj.complex :as c]
            [potemkin :refer [defprotocol+]])
  (:import [org.apache.commons.math3.complex Complex]))

;; the group of mobius transformations, or more precisely, SL(2,C)

(defrecord Transformation [^Complex a ^Complex b ^Complex c ^Complex d])

(defn make-transformation
  [a b c d]
  (let [scale (-> (c/- (c/* a d) (c/* b c)) c/sqrt c/reciprocal)]
    (assert (not (c/zero? scale))
            (format "illegal transformation, determinant zero: a=%s, b=%s, c=%s, d=%s" a b c d))
    (->Transformation (c/* a scale)
                      (c/* b scale)
                      (c/* c scale)
                      (c/* d scale))))

(def unit (->Transformation c/one c/zero c/zero c/one))

(defn compose
  ([] unit)
  ([s] s)
  ([{a1 :a b1 :b c1 :c d1 :d} {a2 :a b2 :b c2 :c d2 :d}]
   (->Transformation (c/+ (c/* a1 a2) (c/* b1 c2))
                     (c/+ (c/* a1 b2) (c/* b1 d2))
                     (c/+ (c/* c1 a2) (c/* d1 c2))
                     (c/+ (c/* c1 b2) (c/* d1 d2))))
  ([s t & more]
   (reduce compose (compose s t) more)))

(defn inverse
  [{:keys [a b c d]}]
  (->Transformation d (c/- b) (c/- c) a))

(defprotocol+ Transformable
  (transform [thing transformation]))

(extend-type Complex
  Transformable
  (transform [z {:keys [a b c d]}]
    (c/div (c/+ (c/* a z) b)
           (c/+ (c/* c z) d))))

(defn conjugate
  [s t]
  (compose t s (inverse t)))

(defn trace
  [{:keys [a d]}]
  (c/+ a d))

(defn fixed-points
  [{:keys [a b c d] :as t}]
  ;; z = (az+b)/(cz+d)
  ;; => 0 = cz^2 + (d-a)z - b
  ;; => z = ((a-d) +/- sqrt((d-a)^2 + 4bc))/2c
  ;; given ad-bc=1, we have
  ;; z = (a - d +/- sqrt(trace^2 - 4)) / 2c
  ;;   = (a-d)/2c +/- sqrt(trace^2-4)/2c
  (let [tr (c/+ a d)
        tr-sq (c/* tr tr)
        c2 (c/*real c 2)
        x (c/div (c/- a d) c2)
        y (c/div (c/sqrt (c/-real tr-sq 4)) c2)]
    (if (= tr-sq (c/rect 4 0))
      [x]
      [(c/+ x y) (c/- x y)])))
