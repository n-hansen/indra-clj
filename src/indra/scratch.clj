(ns indra.scratch
  (:require [clojure2d.core :as c2d]
            [clojure2d.color :as color]
            [indra.geometry :as g]
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
  (render* [this canvas fill?]))

(extend-protocol Renderable
  Complex
  (render* [z canvas _]
    (let [[x y] (z->xy z)]
      (c2d/point canvas x y)))

  indra.geometry.Path
  (render* [{:keys [points]} canvas fill?]
    (c2d/path canvas (map z->xy points) false (not fill?)))

  indra.geometry.Circle
  (render* [{:keys [^Complex center ^double radius]} canvas fill?]
    (let [[x y] (z->xy center)]
      (c2d/ellipse canvas x y (* radius ^double scale 2) (* radius ^double scale 2) (not fill?)))))

(defn stroke
  [canvas thing]
  (render* thing canvas false))

(defn fill
  [canvas thing]
  (render* thing canvas true))

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

(defn make-window
  [f]
  (c2d/show-window
   {:window-name "indra"
    :canvas (c2d/canvas 600 600 :high)
    :draw-fn f}))

(defn square-inversion
  [canvas window framecount _]
  (binding [g/*max-path-segment-length* 0.005]
    (let [shape (g/->Path [(c/rect 0.2 0.5)
                           (c/rect 1.1 0.5)
                           (c/rect 1.1 1.4)
                           (c/rect 0.2 1.4)
                           (c/rect 0.2 0.5)])]
      (-> canvas
          set-up-canvas
          (c2d/set-color (color/color 50 100 0))
          (stroke shape)
          (c2d/set-awt-color (color/awt-color 100 200 50))
          (stroke (m/transform shape r/inversion))))))

(comment
  (make-window #'square-inversion)
  )

(defn loxodromic-map
  [canvas _ _ _]
  (set-up-canvas canvas)
  (let [t (m/conjugate (r/pure-scaling (c/rect 1.0 0.4))
                       (m/make-transformation c/one (c/- c/one) c/one c/one))
        t-inv (m/inverse t)]
    (c2d/set-stroke canvas 5)
    (c2d/set-color canvas (color/color :black))
    (loop [z- c/zero
           z+ c/zero
           n 100]
      (stroke canvas z-)
      (stroke canvas z+)
      (when (pos? n)
        (recur (m/transform z- t-inv)
               (m/transform z+ t)
               (dec n))))))

(comment
  (make-window #'loxodromic-map)
  )
