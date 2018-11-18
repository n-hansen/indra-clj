(ns indra.explorer.render
  (:require [clojure.java.io :as io]
            [indra.complex :as c]
            [indra.limit-sets.render :as lsr]
            [indra.mobius.recipes :as recipes])
  (:import [java.io ByteArrayOutputStream]))

(defn clamped-number-parser
  [parser]
  (fn [x low high] (-> x
                       parser
                       (max low)
                       (min high))))

(def parse-clamped-long
  (clamped-number-parser #(Long/parseLong %)))

(def parse-clamped-double
  (clamped-number-parser #(Double/parseDouble %)))

(defn render-limit-set
  [{:keys [ax ay bx by size depth epsilon] :as query-params}]
  (when (and ax ay bx by size depth epsilon)
    (let [group (recipes/parabolic-commutator-group
                 (c/rect (Double/parseDouble ax) (Double/parseDouble ay))
                 (c/rect (Double/parseDouble bx) (Double/parseDouble by)))
          params (merge group
                        {:width (parse-clamped-long size 10 300)
                         :height (parse-clamped-long size 10 300)
                         :depth (parse-clamped-long depth 6 200)
                         :epsilon (parse-clamped-double epsilon 3.0 0.75)
                         :color-step 4e-5
                         :special-repetends [[:a] [:b] [:A] [:B]]
                         :timeout 700})
          img (lsr/render-limit-set params)]
      (with-open [os (ByteArrayOutputStream.)]
        (lsr/write-to-png img os)
        (.flush os)
        (.toByteArray os)))))

(defn successful-png-response
  [bytes]
  {:status 200
   :body bytes
   :headers {"Content-Type" "image/png"}})

(defn handler
  [{:keys [query-params] :as request}]
  (if-some [response (some->> (render-limit-set query-params)
                              (successful-png-response))]
    response
    {:status 500
     :body "That didn't do what you thought it would..."}))
