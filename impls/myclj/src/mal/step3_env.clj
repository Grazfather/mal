(ns mal.step3-env
  (:require [clojure.repl]
            [mal.env :as env]
            [mal.readline :as readline]
            [mal.reader :as reader]
            [mal.printer :as printer])
  (:gen-class))

(def repl-env (env/env-new))
(mapv #(env/env-set! %1 %2 repl-env)
      [(symbol "+") (symbol "-")
       (symbol "*")
       (symbol "/")]
      [+ - * quot])

(defn READ [s]
  (reader/read-str s))

(declare eval-ast)

(defn EVAL [ast env]
  (if (seq? ast)
    (if (empty? ast)
      ast
      (let [[kword a1 a2] ast]
        (condp = kword
          'def! (env/env-set! a1 (EVAL a2 env) env)
          'let* (let [let-env (env/env-new env)]
                  (doseq [[k v] (partition 2 a1)]
                    (env/env-set! k (EVAL v let-env) let-env))
                  (EVAL a2 let-env))
          (let [new-ast (eval-ast ast env)]
            (apply (first new-ast) (rest new-ast))))))
    (eval-ast ast env)))

(defn eval-ast
  [ast env]
  (cond
    (symbol? ast) (env/env-get ast env)
    (seq? ast) (map #(EVAL % env) ast)
    (vector? ast) (mapv #(EVAL % env) ast)
    :else ast))

(defn PRINT [arg]
  (printer/pr-str arg))

(defn rep [arg]
  (-> arg
      READ
      (EVAL repl-env)
      PRINT))

(defn repl-loop []
  (print "user> ")
  (flush)
  (when-let [l (readline/readline "")]
    (try
      (println (rep l))
      (catch Throwable e (clojure.repl/pst e)))
    (recur)))

(defn -main [& args]
  (repl-loop))
