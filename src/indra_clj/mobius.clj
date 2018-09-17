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
  [{a1 :a b1 :b c1 :c d1 :d} {a2 :a b2 :b c2 :c d2 :d}]
  (->Transformation (c/+ (c/* a1 a2) (c/* b1 c2))
                    (c/+ (c/* a1 b2) (c/* b1 d2))
                    (c/+ (c/* c1 a2) (c/* d1 c2))
                    (c/+ (c/* c1 b2) (c/* d1 d2))))

(defn inverse
  [{:keys [a b c d]}]
  (->Transformation d (c/- b) (c/- c) a))

(defprotocol+ Transformable
  (transform [thing transformation]))
