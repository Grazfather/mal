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
                     :aot [mal.step1-read-print]}
             :step2 {:main mal.step2-eval
                     :uberjar-name "step2_eval.jar"
                     :aot [mal.step2-eval]}
             :step3 {:main mal.step3-env
                     :uberjar-name "step3_env.jar"
                     :aot [mal.step3-env]}
             :step4 {:main mal.step4-if-fn-do
                     :uberjar-name "step4_if_fn_do.jar"
                     :aot [mal.step4-if-fn-do]}})
