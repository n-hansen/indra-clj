(ns indra.geometry.render-c2d
  (:require [clojure2d.core :as c2d]
            [indra.complex :as c]
            [indra.geometry :as g]
            [potemkin :refer [defprotocol+]])
  (:import [org.apache.commons.math3.complex Complex]))

;; rendering with the clojure2d wrapper to java2d

(defprotocol+ Renderable
  (render* [this canvas]))

(extend-protocol Renderable
  Complex
  (render* [z canvas]
    (c2d/point canvas (c/real z) (c/imag z)))

  indra.geometry.Path
  (render* [{:keys [points]} canvas]
    (->> points
         (map (fn [z] [(c/real z) (c/imag z)]))
         (c2d/path canvas)))

  indra.geometry.Circle
  (render* [{:keys [^Complex center ^double radius]} canvas]
    (c2d/ellipse (c/real center) (c/imag center) (/ radius 2) (/ radius 2))))

(defn render
  ; convenience function for clojure2d.core/with-canvas->
  [canvas thing]
  (render* thing canvas))
