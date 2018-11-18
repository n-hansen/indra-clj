(defproject indra "0.1.0-SNAPSHOT"
  ;; :description "FIXME: write description"
  ;; :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[cheshire "5.8.1"]
                 [clojure2d "1.0.2"]
                 [net.cgrand/xforms "0.18.2"]
                 [org.clojure/clojure "1.9.0"]
                 [org.apache.commons/commons-math3 "3.6.1"]
                 [potemkin "0.4.5"]

                 ;; web nonsense
                 [io.pedestal/pedestal.service       "0.5.4"]
                 [io.pedestal/pedestal.route         "0.5.4"]
                 #_[io.pedestal/pedestal.service-tools "0.5.4"] ;; Only needed for ns-watching; WAR tooling
                 [io.pedestal/pedestal.jetty         "0.5.4"]
                 #_[io.pedestal/pedestal.immutant      "0.5.4"]
                 #_[io.pedestal/pedestal.tomcat        "0.5.4"]
                 #_[io.pedestal/pedestal.aws           "0.5.4"] ;; API-Gateway, Lambda, and X-Ray support
                 ]
  :main ^:skip-aot indra.core
  :source-paths ["src/clj"]
  :test-paths ["test"]
  :resource-paths ["resources"]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[criterium "0.4.4"]]}})
