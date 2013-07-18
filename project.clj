(defproject heyawake "0.1.0-SNAPSHOT"
  :description "A simple heyawake puzzle solution verifier"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.nrepl "0.2.3"]
                 [org.clojure/core.logic "0.8.3"]
                 [ring "1.2.0"]
                 [domina "1.0.1"]
                 [org.clojure/google-closure-library-third-party "0.0-2029"]]
  :plugins [[lein-cljsbuild "0.2.8"]]
  :hooks [leiningen.cljsbuild]
  :cljsbuild { 
    :builds {
     :main {
      :source-path "cljs"
      :compiler
        {
          :output-to "resources/public/js/cljs.js"
          :optimizations :simple
          :pretty-print true
        }
        :jar true
      }
    }
  }
  :main heyawake.server)
