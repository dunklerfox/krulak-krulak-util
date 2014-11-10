(defproject krulak/krulak-util "0.1.0-SNAPSHOT"
  :description "Krulak util library"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [cheshire "5.3.0"]
                 [clj-time "0.6.0"]
                 [com.cemerick/url "0.1.0"]
                 [ring/ring-core "1.2.1"]
                 [org.clojure/core.cache "0.6.4"]]
  
  :min-lein-version "2.0.0"

  :main krulak.util)
