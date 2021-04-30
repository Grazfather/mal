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
      (condp = (first ast)
        (symbol "def!") (env/env-set! (second ast) (EVAL (nth ast 2) env) env)
        (symbol "let*") (let [newenv (env/env-new env)
                              bindings (second ast)]
                          (loop [k (first bindings)
                                 v (second bindings)
                                 remaining (drop 2 bindings)]
                            (env/env-set! k (EVAL v newenv) newenv)
                            (when-not (empty? remaining)
                              (recur (first remaining) (second remaining) (drop 2 remaining))))
                          (EVAL (nth ast 2) newenv))
        (let [new-ast (eval-ast ast env)]
          (apply (first new-ast) (rest new-ast)))))
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
