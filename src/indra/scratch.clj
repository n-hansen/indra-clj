(ns indra.scratch
  (:require [clojure2d.core :as c2d]
            [clojure2d.color :as color]
            [indra.geometry :as g]
            [indra.mobius :as m]
            [indra.mobius.recipes :as r]
            [indra.complex :as c]
            [indra.schottky :as schottky]
            [indra.limit-sets :as ls]
            [potemkin :refer [defprotocol+]])
  (:import [org.apache.commons.math3.complex Complex]
           [org.apache.commons.math3.util FastMath]))

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
(def scale 150.0)

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
    :draw-fn f
    #_#_:fps 10}))

(defn make-window-next
  [img]
  (c2d/show-window
   {:window-name (str "indra-" (rand-int 1000000))
    :canvas (c2d/canvas 600 600 :high)
    :draw-fn (fn [c _ _ _] (some? @img) (c2d/image c @img))
    #_#_:fps 10}))

(defn render-limit-points
  [limit-set]
  (c2d/with-canvas [canvas (c2d/canvas 600 600)]
    (set-up-canvas canvas)
    (c2d/set-stroke canvas 1)
    (let [s (volatile! 0)]
      (doseq [[p1 p2] (partition 2 1 limit-set)
              :let [c ((color/gradient-presets :iq-1)
                       (vswap! s + (* (c/abs (c/- p1 p2)) 0.03)))]]
       (-> canvas
           (c2d/set-color c)
           (fill p1))))
    (c2d/get-image canvas)))

;; begin examples

(defn circle-inversion-example
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
  (make-window #'circle-inversion-example)
  )

(defn loxodromic-map-example
  [canvas _ _ _]
  (set-up-canvas canvas)
  (let [t (m/conjugate (r/pure-scaling (c/rect 1.0 0.4))
                       (m/make-transformation c/one (c/- c/one) c/one c/one))
        t-inv (m/inverse t)
        init (g/->Path [(c/rect -0.1 -0.15)
                        (c/rect 0 0.1)
                        (c/rect 0.1 -0.1)
                        (c/rect -0.1 -0.15)])]
    (c2d/set-stroke canvas 1)
    (c2d/set-color canvas (color/color :black))
    (stroke canvas init)
    (loop [z- init
           z+ init
           n 12]
      (stroke canvas z-)
      (stroke canvas z+)
      (when (pos? n)
        (recur (m/transform z- t-inv)
               (m/transform z+ t)
               (dec n))))))

(comment
  (make-window #'loxodromic-map-example)
  )

(defn pair-circles-example
  [canvas _ _ _]
  (set-up-canvas canvas)
  (let [c1 (g/->Circle c/one 0.8)
        c2 (g/->Circle (c/- c/one) 0.7)
        t (r/pair-circles c1 c2)
        triangle (g/->Path [(c/rect -0.1 -0.2)
                            (c/rect 0 0.1)
                            (c/rect 0.1 -0.1)
                            (c/rect -0.1 -0.2)])]
    (-> canvas
        (c2d/set-color :blue)
        (stroke c1)
        (c2d/set-color :blue-violet)
        (stroke c2)
        (c2d/set-color :forest-green)
        (stroke triangle)
        (c2d/set-color :cornflower-blue)
        (stroke (m/transform triangle t))
        (c2d/set-color :dark-orange)
        (stroke (m/transform triangle (m/inverse t))))))

(comment
  (make-window #'pair-circles-example)
  )

(defn schottky-example
  [canvas _ _ _]
  (set-up-canvas canvas)
  (let [c1 (g/->Circle (c/rect 1.7 1) 1.2)
        c2 (g/->Circle (c/rect -1.6 -1.1) 1.35)
        c3 (g/->Circle (c/rect 1.2 -1.4) 1.2)
        c4 (g/->Circle (c/rect -0.9 1.2) 1)
        disks (schottky/schottkey-disks c1 c2 c3 c4 5
                                        (m/compose
                                         (r/pure-rotation 2)
                                         (r/special-stretch-map 1.03))
                                        (r/pure-rotation -1))]
    (doseq [{:keys [depth disk]} disks]
      (-> canvas
          (c2d/set-color (case (int depth)
                           1 :red
                           2 :orange
                           3 :yellow
                           4 :magenta
                           :green))
          (fill disk)))))

(comment
  (make-window #(schottky-example %1 %2 %3 %4))
  )

(defn fuchsian-example-1
  [canvas _ _ _]
  (set-up-canvas canvas)
  (let [s 0.385
        t 0.9
        s* (double (/ s))
        t* (double (/ t))
        ca (g/->Circle (c/rect (/ (+ s t) 2) 0) (/ (- t s) 2))
        cA (g/->Circle (c/rect (/ (+ s t) -2) 0) (/ (- t s) 2))
        cb (g/->Circle (c/rect (/ (+ s* t*) 2) 0) (/ (- s* t*) 2))
        cB (g/->Circle (c/rect (/ (+ s* t*) -2) 0) (/ (- s* t*) 2))
        disks (schottky/schottkey-disks ca cA cb cB 3)]
    (doseq [{:keys [word disk]} disks]
      (-> canvas
          (c2d/set-color (case (first word)
                           :a :magenta
                           :A :turquoise
                           :b :orange
                           :B :dark-red))
          (fill disk)
          (c2d/set-color :black)
          (stroke disk)))))

(comment
  (make-window #'fuchsian-example-1)
  )

(def limit-set-1
  (let [c1 (g/->Circle (c/rect 1.7 1) 1.2)
        c2 (g/->Circle (c/rect -1.6 -1.1) 1.35)
        c3 (g/->Circle (c/rect 1.2 -1.4) 1.2)
        c4 (g/->Circle (c/rect -0.9 1.2) 1)
        [a a* b b*] (schottky/pairing-transforms [c1 c2 (m/compose
                                                         (r/pure-rotation 1.9)
                                                         (r/special-stretch-map 1.03))]
                                                 [c3 c4 (r/pure-rotation -1)])
        repetends  [[:a] [:b] [:A] [:B] [:a :b :A :B]]]
    (into [] (ls/limit-set-fixed-depth-dfs a a* b b* repetends 6))))

(defn limit-set-example-1
  [canvas _ _ _]
  (-> (set-up-canvas canvas)
      (c2d/set-stroke 2))
  (doseq [[ix p] (map-indexed vector limit-set-1)
          :let [c ((color/gradient-presets :iq-1) (/ ix (dec (count limit-set-1))))]]
    (-> canvas
        (c2d/set-color c)
        (fill p))))

(comment
  (make-window #'limit-set-example-1)
  )

;; kissing fuchsian limit set

(def fuchsian-example-2-limit-set
  (let [sqrt2 (c/rect (FastMath/sqrt 2) 0)
        a (m/make-transformation sqrt2     c/i
                                 (c/- c/i) sqrt2)
        a* (m/inverse a)
        b (m/make-transformation sqrt2 c/one
                                 c/one sqrt2)
        b* (m/inverse b)
        repetends  [[:a] [:b] [:A] [:B] [:a :b :A :B]]]
    (into [] (ls/limit-set-fixed-depth-dfs a a* b b* repetends 7))))

(defn fuchsian-example-2
  [canvas _ _ _]
  (-> (set-up-canvas canvas)
      (c2d/set-stroke 2))
  (doseq [[ix p] (map-indexed vector fuchsian-example-2-limit-set)
          :let [c ((color/gradient-presets :iq-1) (/ ix (dec (count fuchsian-example-2-limit-set))))]]
    (-> canvas
        (c2d/set-color c)
        (fill p))))

(comment
  (make-window #'fuchsian-example-2)
  )

;; perturbation of a fuchsian group

(def limit-set-example-2-limit-set
  (let [sqrt2 (c/rect (FastMath/sqrt 1.9) 0.4)
        a (m/make-transformation sqrt2     c/i
                                 (c/- c/i) sqrt2)
        a* (m/inverse a)
        b (m/make-transformation sqrt2 c/one
                                 c/one sqrt2)
        b* (m/inverse b)
        repetends  [#_#_#_#_[:a] [:b] [:A] [:B] [:a :b :A :B] [:b :A :B :a] [:A :B :a :b] [:B :a :b :A]]]
    (into [] (ls/limit-set-fixed-depth-dfs a a* b b* repetends 10))))

(defn limit-set-example-2
  [canvas _ _ _]
  (-> (set-up-canvas canvas)
      (c2d/set-stroke 1))
  (doseq [[ix p] (map-indexed vector limit-set-example-2-limit-set)
          :let [c ((color/gradient-presets :iq-1) (/ ix (dec (count limit-set-example-2-limit-set))))]]
    (-> canvas
        (c2d/set-color c)
        (fill p))))

(comment
  (make-window #'limit-set-example-2)
  )

;; quasi-fuchsian group

(defonce quasi-fuchsian-example-1-disk-image (atom nil))
(defonce quasi-fuchsian-example-1-points-image (atom nil))

(defn quasi-fuchsian-example-1-render
  []
  (let [disk-depth 8
        point-depth 40
        y 0.95
        x (FastMath/sqrt (+ 1 (* y y)))
        k 1.4
        v (/ (* y (/ (+ k (/ k)) 2)))
        u (FastMath/sqrt (+ 1 (* v v)))
        a (m/make-transformation (c/rect x 0) (c/rect y 0)
                                 (c/rect y 0) (c/rect x 0))
        a* (m/inverse a)
        b (m/make-transformation (c/rect u 0)           (c/rect 0 (* k v))
                                 (c/rect 0 (- (/ v k))) (c/rect u 0))
        b* (m/inverse b)

        ca  (g/->Circle (c/rect    (/ x y)  0) (/ y))
        ca* (g/->Circle (c/rect (- (/ x y)) 0) (/ y))
        cb  (g/->Circle (c/rect 0    (/ (* k u) v))  (/ k v))
        cb* (g/->Circle (c/rect 0 (- (/ (* k u) v))) (/ k v))

        disks (schottky/schottkey-disks ca ca* cb cb* disk-depth {:a a :A a* :b b :B b*})

        limit-set (ls/limit-set-dfs a b point-depth 5e-3 [[:a] [:b] [:A] [:B]])]
    (reset! quasi-fuchsian-example-1-points-image (render-limit-points limit-set))
    (reset! quasi-fuchsian-example-1-disk-image
            (c2d/with-canvas [canvas (c2d/canvas 600 600)]
              (set-up-canvas canvas)
              (doseq [{:keys [depth disk]} disks]
                (-> canvas
                    (c2d/set-color ((color/gradient-presets :iq-6)
                                    (mod (/ depth 12.0) 1)))
                    (fill disk)))
              (c2d/get-image canvas)))))

(comment
  (make-window-next quasi-fuchsian-example-1-disk-image)
  (make-window-next quasi-fuchsian-example-1-points-image)
  (quasi-fuchsian-example-1-render)
  )

;; apollonian gasket, half plane projection

(defonce apollonian-image (atom nil))

(defn apollonian-image-render
  []
  (let [depth 50
        epsilon 5e-3
        a (m/make-transformation c/one         c/zero
                                 (c/rect 0 -2) c/one)
        b (m/make-transformation (c/rect 1 -1) c/one
                                 c/one         (c/rect 1 1))
        limit-set (ls/limit-set-dfs a b depth epsilon
                                    [[:a] [:b] [:A] [:B]])]
    (reset! apollonian-image (render-limit-points (map #(c/*real % 2) limit-set)))))

(comment
  (make-window-next apollonian-image)
  (apollonian-image-render)
  )
