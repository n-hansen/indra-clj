(ns indra.geometry
  (:require [indra.complex :as c]
            [indra.mobius :as m]
            [net.cgrand.xforms :as x])
  (:import [org.apache.commons.math3.complex Complex]))

;; various Transformable objects

(def ^:dynamic *max-path-segment-length* 1.0)

(defrecord Path [points]
  m/Transformable
  (m/transform [this t]
    (into [] (comp (x/partition 2 1)
                   (mapcat (fn [[p q]]
                             (let [p* (m/transform p t)
                                   q* (m/transform q t)
                                   length (c/abs (c/- p* q*))]
                               (if (< length ^double *max-path-segment-length*)
                                 [p* q*]
                                 ; if we will transform into a segment that is too long,
                                 ; instead subdivide the segment before transforming
                                 (let [subsegments (-> length
                                                       (quot ^double *max-path-segment-length*)
                                                       (inc)
                                                       (* 2))
                                       d (c/div-real (c/- q p) (double subsegments))]
                                   (-> [p*]
                                       (into (map #(m/transform (c/+ p (c/*real d (double %))) t))
                                             (range 1 subsegments))
                                       (conj q*))))))))
          points)))
