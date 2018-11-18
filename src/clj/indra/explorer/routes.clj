(ns indra.explorer.routes
  (:require [indra.explorer.render :as render]
            [io.pedestal.http.route :as route]))

(def routes
  (route/expand-routes
   #{["/render" :get render/handler :route-name :render]}))
