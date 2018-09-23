(ns indra.geometry.render
  (:require [indra.complex :as c]
            [indra.geometry :as g]
            [potemkin :refer [defprotocol+]])
  (:import [java.awt.geom Path2D$Double Ellipse2D$Double]
           [org.apache.commons.math3.complex Complex]))

(defprotocol+ Renderable
  (get-shape [this]))

(extend-protocol Renderable
  indra.geometry.Path
  (get-shape [{:keys [points]}]
    (let [[head & tail] points
          path (doto (Path2D$Double.)
                 (.moveTo (c/real head) (c/imag head)))]
      (doseq [pt tail]
        (.lineTo path (c/real pt) (c/imag pt)))
      path))

  indra.geometry.Circle
  (get-shape [{:keys [^Complex center ^double radius]}]
    (Ellipse2D$Double. (- (c/real center) radius)
                       (- (c/imag center) radius)
                       (* 2 radius)
                       (* 2 radius))))
