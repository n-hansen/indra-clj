(ns indra.geometry
  (:require [indra.complex :as c]
            [indra.mobius :as m]
            [net.cgrand.xforms :as x])
  (:import [org.apache.commons.math3.complex Complex]
           [org.apache.commons.math3.util Precision]))

;; various Transformable objects

(def ^:dynamic *max-path-segment-length* 1.0)

(defrecord Path [points]
  m/Transformable
  (m/transform [this t]
    (->Path
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
           points))))

(declare ->Line)

(defrecord Circle [^Complex center ^double radius]
  m/Transformable
  (m/transform [this {:keys [c d] :as t}]
    (if ; we get a line iff |d/c+P|=r
        (-> (c/div d c)
            (c/+ center)
            c/abs
            (Precision/equals radius))
      (let [[p1 p2] (into [] (comp (map #(-> % (c/+ center) (m/transform t))) ; TODO solve explicitly
                                   (remove c/inf?))
                          [(c/rect radius 0)
                           (c/rect 0 radius)
                           (c/rect (- radius) 0)])]
        (->Line p1 (c/argument (c/- p2 p1))))
      (let [z (c/- center
                   (-> (c/div d c)
                       (c/+ center)
                       c/conjugate
                       c/reciprocal
                       (c/*real (* radius radius))))
            center* (m/transform z t)]
        (->Circle center*
                  (c/abs (c/- center* (m/transform (c/+real center radius) t))))))))

(defrecord Line [^Complex offset ^double direction]
  m/Transformable
  (m/transform [this t]
    (throw (ex-info "line transformations not yet implemented" {}))))


(comment

  (require 'criterium.core)

  (def circ1 (->Circle (c/rect 1 2) 5))

  (criterium.core/quick-bench (c/inf? (c/reciprocal (:center circ1))))

  (def circ2 {:center (c/rect 1 2)
              :radius 5})

  (criterium.core/quick-bench (c/inf? (c/reciprocal (:center circ2))))

  (def circ3 (assoc (->Circle (c/rect 1 2) 5)
                    :butts 3))

  (criterium.core/quick-bench (c/inf? (c/reciprocal (:center circ1))))



  (def the-path (vec (repeatedly 1000 #(c/rect (* 100 (- (rand) 0.5))
                                               (* 100 (- (rand) 0.5))))))
  (def the-transform (m/make-transformation (c/rect 1 -5) c/one c/one (c/rect 1 -5)))

  (defn path-transform-for
    [points t]
    (for [[p q] (partition 2 1 points)
          :let [p* (m/transform p t)
                q* (m/transform q t)
                length (c/abs (c/- p* q*))]
          r (if (< length *max-path-segment-length*)
              [p* q*]
              (let [subsegments (-> (quot length *max-path-segment-length*)
                                    (inc)
                                    (* 2))
                    d (c/div-real (c/- q p) (double subsegments))]
                (for [i (range (inc subsegments))]
                  (m/transform (c/+ p (c/*real d (double i))) t))))]
      r))

  (criterium.core/quick-bench (-> the-path
                                  (path-transform-for the-transform)
                                  (path-transform-for the-transform)
                                  count))

  (criterium.core/quick-bench (-> the-path
                                  (path-transform-xform-1 the-transform)
                                  (path-transform-xform-1 the-transform)
                                  count))

  (defn path-transform-xform-2
    [points t]
    (into [] (comp (x/partition 2 1)
                   (mapcat (fn [[p q]]
                             (let [p* (m/transform p t)
                                   q* (m/transform q t)
                                   length (c/abs (c/- p* q*))]
                               (if (< length *max-path-segment-length*)
                                 [p* q*]
                                 (let [subsegments (-> length ;
                                                       (quot *max-path-segment-length*)
                                                       (inc)
                                                       (* 2))
                                       d (c/div-real (c/- q p) (double subsegments))]
                                   (-> [p*]
                                       (into (map #(m/transform (c/+ p (c/*real d (double %))) t))
                                             (range 1 subsegments))
                                       (conj q*))))))))
          points))

  (criterium.core/quick-bench (-> the-path
                                  (path-transform-xform-2 the-transform)
                                  (path-transform-xform-2 the-transform)
                                  count)))
