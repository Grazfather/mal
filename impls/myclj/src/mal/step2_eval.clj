(ns mal.step2-eval
  (:require [clojure.repl]
            [mal.readline :as readline]
            [mal.reader :as reader]
            [mal.printer :as printer])
  (:gen-class))

(def repl-env
  {(symbol "+") +
   (symbol "-") -
   (symbol "*") *
   (symbol "/") quot})

(defn READ [s]
  (reader/read-str s))

(declare eval-ast)

(defn EVAL [ast env]
  (if (seq? ast)
    (if (empty? ast)
      ast
      (let [new-ast (eval-ast ast env)]
        (apply (first new-ast) (rest new-ast))))
    (eval-ast ast env)))

(defn eval-ast
  [ast env]
  (cond
    (symbol? ast) (if (contains? env ast)
                    (get env ast)
                    (throw (Exception. "Value not found")))
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
