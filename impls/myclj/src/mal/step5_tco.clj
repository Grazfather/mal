(ns mal.step5-tco
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
  (loop [ast ast
         env env]
    (if (seq? ast)
      (if (empty? ast)
        ast
        (let [[kword a1 a2] ast]
          (condp = kword
            'def! (env/env-set! a1 (EVAL a2 env) env)
            'let* (let [let-env (env/env-new env)]
                    (doseq [[k v] (partition 2 a1)]
                      (env/env-set! k (EVAL v let-env) let-env))
                    (recur a2 let-env))
            'do (let [e-ast (butlast ast 1)
                      lst (last ast)]
                  (eval-ast e-ast env)
                  (recur lst env))
            'if (if (EVAL a1 env)
                  (recur a2 env)
                  (when (> (count ast) 3)
                    (recur (nth ast 3) env)))
            'fn* {:func (fn [& args]
                          (EVAL a2 (env/env-new env a1 args)))
                  :expr a2
                  :params a1
                  :env env}
            (let [new-ast (eval-ast ast env)
                  f (first new-ast)
                  args (rest new-ast)]
              (if (map? f)
                (let [{:keys [expr env params]} f
                      f-env (env/env-new env params args)]
                  (recur expr f-env))
                (apply f args))))))
      (eval-ast ast env))))

(defn eval-ast
  [ast env]
  (cond
    (symbol? ast) (env/env-get ast env)
    (seq? ast) (map #(EVAL % env) ast)
    (vector? ast) (mapv #(EVAL % env) ast)
    :else ast))

(defn PRINT [arg]
  (printer/pr-str arg true))

(defn rep [arg]
  (-> arg
      READ
      (EVAL repl-env)
      PRINT))

; Define `not` in mal itself
;; (rep "(def! not (fn* (a) (if a false true)))")

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
