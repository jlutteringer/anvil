(defproject alloy/anvil-clojure "0.0.1-SNAPSHOT"
  :plugins [[lein-modules "0.3.11"] [lein-cljsbuild "1.1.1"]]
  :description "FIXME: write description"
  :source-paths ["src/clj" "src/cljs" "src/cljc"]
  :dependencies [
                 [org.clojure/clojure "1.9.0-alpha10"]
                 [org.clojure/clojurescript "1.9.198"]
                 [com.rpl/specter "0.9.2"]]

  :cljsbuild {:builds [{:source-paths ["src/cljc" "src/cljs"]
                        :compiler     {
                                       :output-to     "target/assets/js/alloy/core.js"
                                       :optimizations :whitespace
                                       :pretty-print  true
                                       }}]}
  :clean-targets ^{:protect false} [:target-path "target/assets/"])