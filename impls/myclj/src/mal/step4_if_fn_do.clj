(ns mal.step4-if-fn-do
  (:require [clojure.repl]
            [mal.env :as env]
            [mal.readline :as readline]
            [mal.reader :as reader]
            [mal.printer :as printer]
            [mal.core :as core])
  (:gen-class))

(def repl-env (env/env-new
               nil
               [(symbol "+")
                (symbol "-")
                (symbol "*")
                (symbol "/")]
               [+ - * quot]))

(doseq [[k v] core/ns]
  (env/env-set! k v repl-env))

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
          'do (last (eval-ast (rest ast) env))
          'if (if (EVAL a1 env)
                (EVAL a2 env)
                (when (> (count ast) 3)
                  (EVAL (nth ast 3) env)))
          'fn* (fn [& args]
                 (let [fn-env (env/env-new env a1 args)]
                   (EVAL a2 fn-env)))
          (let [new-ast (eval-ast ast env)]
            (apply (first new-ast) (rest new-ast))))))
    (eval-ast ast env)))

(defn eval-ast
  [ast env]
  (cond
    (symbol? ast) (env/env-get ast env)
    (seq? ast) (mapv #(EVAL % env) ast)
    (vector? ast) (mapv #(EVAL % env) ast)
    (map? ast) (->> ast
                    (apply concat) ; Flatten to vector
                    (map #(EVAL % env))
                    (apply hash-map)) ; Back to a map
    :else ast))

(defn PRINT [arg]
  (printer/pr-str arg true))

(defn rep [arg]
  (-> arg
      READ
      (EVAL repl-env)
      PRINT))

; Define `not` in mal itself
(rep "(def! not (fn* (a) (if a false true)))")

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
