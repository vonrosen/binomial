(defproject binomial "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[cider/cider-nrepl "0.9.1"]
	      [lein2-eclipse "2.0.0"]]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojure-contrib "1.1.0"]]
  #_:profiles #_{:dev {:dependencies [[com.cemerick/piggieback "0.2.0"]
                                     [org.clojure/tools.nrepl "0.2.10"]]
                      :repl-options {:nrepl-middleware
                                     [cemerick.piggieback/wrap-cljs-repl]}}}
  :aot  [binomial.core]
  :main binomial.core)
