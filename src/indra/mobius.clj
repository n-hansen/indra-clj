(ns indra.mobius
  (:require [indra.complex :as c]
            [potemkin :refer [defprotocol+]])
  (:import [org.apache.commons.math3.complex Complex]))

;; the group of mobius transformations

(defrecord Transformation [^Complex a ^Complex b ^Complex c ^Complex d])

(def unit (->Transformation c/one c/zero c/zero c/one))

(defn make-transformation
  "ensures our transformation is a member of SL(2,C)"
  ([{:keys [a b c d]}] (make-transformation a b c d))
  ([a b c d]
   (let [scale (-> (c/- (c/* a d) (c/* b c)) c/sqrt c/reciprocal)]
     (assert (not (c/zero? scale))
             (format "illegal transformation, determinant zero: a=%s, b=%s, c=%s, d=%s" a b c d))
     (->Transformation (c/* a scale)
                       (c/* b scale)
                       (c/* c scale)
                       (c/* d scale)))))

(defn scale
  [{:keys [a b c d]} z]
  (->Transformation (c/* a z)
                    (c/* b z)
                    (c/* c z)
                    (c/* d z)))

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

(defn =*
  ([s t] (=* s t 1e-9))
  ([s t e]
   (every? #(c/=* (% s) (% t) e) [:a :b :c :d])))

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

(defn determinant
  [{:keys [a b c d]}]
  (c/- (c/* a d) (c/* b c)))

(defn trace
  [{:keys [a d]}]
  (c/+ a d))

(defn fixed-points
  [{:keys [a b c d] :as t}]
  ;; z = (az+b)/(cz+d)
  ;; => 0 = cz^2 + (d-a)z - b
  ;; => z = ((a-d) +/- sqrt((d-a)^2 + 4bc))/2c
  (let [c2 (c/*real c 2.0)
        lh (c/- a d)
        d-a (c/- d a)
        rh (c/sqrt (c/+ (c/* d-a d-a) (c/*real (c/* b c) 4.0)))]
    [(c/div (c/+ lh rh) c2)
     (c/div (c/- lh rh) c2)]))