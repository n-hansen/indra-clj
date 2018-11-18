(ns indra.limit-sets.render
  (:require [clojure2d.core :as c2d]
            [clojure2d.color :as color]
            [indra.geometry :as g]
            [indra.mobius :as m]
            [indra.mobius.recipes :as r]
            [indra.complex :as c]
            [indra.limit-sets :as ls]
            [potemkin :refer [defprotocol+]]
            [clojure.java.io :as io])
  (:import [org.apache.commons.math3.complex Complex]
           [org.apache.commons.math3.util FastMath]
           [java.awt.image BufferedImage]
           [java.awt Color]
           [java.io ByteArrayOutputStream]
           [javax.imageio ImageIO]))

(def background (color/color 255 255 255))

(def compute-coc-max-depth 8)
(def compute-coc-epsilon 1e-5)
(def coc-margin-pct 1.2)

(defn change-of-coords-old
  ; TODO might be better to handle this with a conjugation
  [width height a b]
  (let [[min-x min-y max-x max-y] (repeatedly #(volatile! 0.0))
        _ (doseq [z (ls/limit-set-dfs a b compute-coc-max-depth compute-coc-epsilon)
                  :let [x (c/real z)
                        y (c/imag z)]]
            (vswap! min-x min x)
            (vswap! max-x max x)
            (vswap! min-y min y)
            (vswap! max-y max y))
        min-x (FastMath/floor @min-x)
        min-y (FastMath/floor @min-y)
        max-x (FastMath/ceil @max-x)
        max-y (FastMath/ceil @max-y)
        real-width (- max-x min-x)
        real-mid-x (+ min-x (/ real-width 2.0))
        scale-x (/ (double width) real-width coc-margin-pct)
        real-height (- max-y min-y)
        real-mid-y (+ min-y (/ real-height 2.0))
        scale-y (/ (double height) real-height coc-margin-pct)
        scale (min scale-x scale-y)]
    {:z->x #(-> % c/real (- real-mid-x) (* scale))
     :z->y #(-> % c/imag (- real-mid-y) (* scale))
     :scale scale}))

(defn change-of-coords
  ; TODO might be better to handle this with a conjugation
  [width height a b]
  (let [[min-x min-y max-x max-y] (repeatedly #(volatile! 0.0))
        _ (doseq [z (ls/limit-set-dfs a b compute-coc-max-depth compute-coc-epsilon)
                  :let [x (c/real z)
                        y (c/imag z)]]
            (vswap! min-x min x)
            (vswap! max-x max x)
            (vswap! min-y min y)
            (vswap! max-y max y))
        ; this is garbage
        min-x (FastMath/floor @min-x)
        min-y (FastMath/floor @min-y)
        max-x (FastMath/ceil @max-x)
        max-y (FastMath/ceil @max-y)
        real-width (- max-x min-x)
        real-mid-x (+ min-x (/ real-width 2.0))
        scale-x (/ (double width) real-width coc-margin-pct)
        real-height (- max-y min-y)
        real-mid-y (+ min-y (/ real-height 2.0))
        scale-y (/ (double height) real-height coc-margin-pct)
        scale (min scale-x scale-y)
        x-offset (int (/ width 2))
        y-offset (int (/ height 2))]
    {:z->x #(-> % c/real (- real-mid-x) (* scale) int (+ x-offset) (min (dec width)) (max 0))
     :z->y #(-> % c/imag (- real-mid-y) (* scale) int (+ y-offset) (min (dec height)) (max 0))
     :scale scale}))

(defn draw-grid!
  [canvas {:keys [scale] :as coc}]
  (let [max-x (/ ^long (c2d/width canvas) 2)
        max-y (/ ^long (c2d/height canvas) 2)]
    (-> canvas
        (c2d/set-background background)
        (c2d/set-stroke 0.5)
        ;; center the canvas on (0,0)
        (c2d/translate max-x max-y)
        (c2d/set-color (color/gray 200)))
    (doseq [x (range 0 max-x scale)]
      (c2d/line canvas x max-y x (- ^long max-y))
      (c2d/line canvas (- x) max-y (- x) (- ^long max-y)))
    (doseq [y (range 0 max-y scale)]
      (c2d/line canvas max-x y (- ^long max-x) y)
      (c2d/line canvas max-x (- y) (- ^long max-x) (- y)))))

(defn render-limit-set
  [{:keys [width height a b depth epsilon-px special-repetends quality color-step]
    :or {width 100 height 100
         depth 20
         epsilon-px 0.8
         quality :high
         color-step 0.0001}}]
  (let [{:keys [z->x z->y scale] :as coc} (change-of-coords width height a b)
        curr-color (volatile! 0.0)]
    (c2d/with-canvas [canvas (c2d/canvas width height quality)]
      #_(draw-grid! canvas coc)
      (c2d/set-stroke canvas 1 :round :round)
      (doseq [z (ls/limit-set-dfs a b depth (/ epsilon-px scale) special-repetends)
              :let [^Color c (color/awt-color
                              ((color/gradient-presets :iq-1)
                               (vswap! curr-color #(mod (+ % color-step) 1.0))))
                    x (z->x z)
                    y (z->y z)
                    ^BufferedImage buffer (:buffer canvas)]]

        (.setRGB buffer x y (.getRGB c))
        #_(-> canvas
              (c2d/set-color c)
              (c2d/rect (-> z z->x int) (-> z z->y int) 1 1)))
      (c2d/get-image canvas))))

(defn write-to-png
  [img os]
  (ImageIO/write img "PNG" os))

(comment
  (require 'criterium.core)
  (require 'indra.mobius.recipes)

  (#_criterium.core/quick-bench
   time
   (let [params {:width 500 :height 500
                 :depth 200
                 :epsilon-px 1.0
                 :color-step 4e-5
                 :special-repetends [[:a] [:b] [:A] [:B]]}
         group (indra.mobius.recipes/parabolic-commutator-group
                (c/rect 1.87 0.1)
                (c/rect 1.87 -0.1))]
     (-> (merge params group)
         render-limit-set
         (c2d/save "renders/test.png")))
   )

  (.next (ImageIO/getImageWritersByFormatName "PNG"))

  (time
   (with-open [os #_(ByteArrayOutputStream.) (io/output-stream "out.png")]
     (let [params {:width 300 :height 300
                   :depth 200
                   :epsilon-px 0.8
                   :color-step 4e-5
                   :special-repetends [[:a] [:b] [:A] [:B]]}
           group (indra.mobius.recipes/parabolic-commutator-group
                  (c/rect 1.87 0.1)
                  (c/rect 1.87 -0.1))]
       (-> (merge params group)
           render-limit-set
           (write-to-png os)))
     #_(.flush ba)
     #_(spit "out.png" ba)))


  )
