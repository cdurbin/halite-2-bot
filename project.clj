(defproject hlt "0.1.0-SNAPSHOT"
  :description "FIXME: My halite bot"
  :url "http://example.com/FIXME"
  :license {:name "All Rights Reserved"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot Durbinator
  :target-path "target/"
  :uberjar-name "MyBot.jar"
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[org.clojure/tools.namespace "0.2.10"]
                                  ; [org.clojars.gjahad/debug-repl "0.3.3"]
                                  [proto-repl "0.3.1"]
                                  [criterium "0.4.4"]]
                   :jvm-opts ^:replace ["-Dcom.sun.management.jmxremote"
                                        "-Dcom.sun.management.jmxremote.ssl=false"
                                        "-Dcom.sun.management.jmxremote.authenticate=false"
                                        "-Dcom.sun.management.jmxremote.port=1198"]
                   :source-paths ["src" "dev" "test"]}})
