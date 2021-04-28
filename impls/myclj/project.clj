(defproject mal "0.1.0-SNAPSHOT"
  :description "Make a Lisp"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [net.n01se/clojure-jna "1.0.0"]]
  :repl-options {:init-ns mal.core}
  :profiles {:step0 {:main mal.step0-repl
                     :uberjar-name "step0_repl.jar"
                     :aot [mal.step0-repl]}
             :step1 {:main mal.step1-read-print
                     :uberjar-name "step1_read_print.jar"
                     :aot [mal.step1-read-print]}})
