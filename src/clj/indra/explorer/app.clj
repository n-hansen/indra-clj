(ns indra.explorer.app
  (:gen-class)
  (:require [indra.explorer.routes :as routes]
            [io.pedestal.http :as http]))

(def ^:const port 8420)

(defn create-server
  []
  (http/create-server
   {::http/routes routes/routes
    ::http/type   :jetty
    ::http/port   port}))

(defn start
  []
  (http/start (create-server)))

(defn -main
  [& args]
  (println "\nStarting server on port" port ". . .")
  (start))
