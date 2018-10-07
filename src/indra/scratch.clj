(ns indra.scratch
  (:require [clojure2d.core :as c2d]
            [clojure2d.color :as color]
            [indra.geometry :as g]
            [indra.geometry.render-c2d :refer [render]]
            [indra.mobius :as m]
            [indra.mobius.recipes :as r]
            [indra.complex :as c]
            [potemkin :refer [defprotocol+]])
  (:import [org.apache.commons.math3.complex Complex]))

;; make everything really annoying forever
(set! *warn-on-reflection* true)
(set! *unchecked-math* false #_:warn-on-boxed)

(def background (color/color 255 255 255))

;; clojure2d canvases seem to work best on the scale of 1 unit = 1 px,
;; but all our math works best close to the origin (e.g. we would like
;; to perform inversions about the unit circle, etc.). clojure2d
;; provides a scaling procedure but it doesn't really work that well
;; for our use case, so we perform scaling by hand before passing to
;; the various rendering functions
(def scale 100.0)

(defn z->xy
  [z]
  [(* ^double scale (c/real z))
   (* ^double scale (c/imag z))])

(defprotocol+ Renderable
  (render* [this canvas]))

(extend-protocol Renderable
  Complex
  (render* [z canvas]
    (let [[x y] (z->xy z)]
      (c2d/point canvas x y)))

  indra.geometry.Path
  (render* [{:keys [points]} canvas]
    (c2d/path canvas (map z->xy points) false true))

  indra.geometry.Circle
  (render* [{:keys [^Complex center ^double radius]} canvas]
    (let [[x y] (z->xy center)]
      (c2d/ellipse canvas x y (* radius ^double scale 2) (* radius ^double scale 2)))))

(defn set-up-canvas
  [canvas]
  (let [max-x (/ ^long (c2d/width canvas) 2)
        max-y (/ ^long (c2d/height canvas) 2)
        draw-grid (fn [canvas]
                    (c2d/set-color canvas (color/gray 200))
                    (doseq [x (range 0 max-x scale)]
                      (c2d/line canvas x max-y x (- ^long max-y))
                      (c2d/line canvas (- x) max-y (- x) (- ^long max-y)))
                    (doseq [y (range 0 max-y scale)]
                      (c2d/line canvas max-x y (- ^long max-x) y)
                      (c2d/line canvas max-x (- y) (- ^long max-x) (- y)))
                    canvas)]
    (-> canvas
       (c2d/set-background background)
       (c2d/set-stroke 1)
       ;; center the canvas on (0,0)
       (c2d/translate max-x max-y)
       draw-grid)))

(def shape
  (g/->Path [(c/rect 0.5 0.5)
             (c/rect 1.1 0.5)
             (c/rect 1.1 1.1)
             (c/rect 0.5 1.1)
             (c/rect 0.5 0.5)])
  #_(g/->Circle (c/rect 1 1) 0.8))


(defn draw
  [canvas window framecount _]
  (-> canvas
      set-up-canvas
      (c2d/set-color (color/color 50 100 0))
      (render shape)
      (c2d/set-awt-color (color/awt-color 100 200 50))
      (render (m/transform shape r/inversion))
      ))

(defn make-window
  []
  (c2d/show-window
   {:window-name "indra"
    :canvas (c2d/canvas 600 600 :high)
    :draw-fn #'draw}))

(comment
  (make-window)
  )
