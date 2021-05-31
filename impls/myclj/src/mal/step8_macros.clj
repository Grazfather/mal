(ns mal.step8-macros
  (:refer-clojure :exclude [macroexpand])
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
(declare quasiquote)
(declare macroexpand)

(defn EVAL [ast env]
  (loop [ast ast
         env env]
    (if (seq? ast)
      (let [ast (macroexpand ast env)]
        (if-not (seq? ast)
          (eval-ast ast env)

          (let [[kword a1 a2] ast]
            (condp = kword
              nil ast ; Empty ast
              'def! (env/env-set! a1 (EVAL a2 env) env)
              'let* (let [let-env (env/env-new env)]
                      (doseq [[k v] (partition 2 a1)]
                        (env/env-set! k (EVAL v let-env) let-env))
                      (recur a2 let-env))
              'do (let [e-ast (butlast (drop 1 ast))
                        lst (last ast)]
                    (eval-ast e-ast env)
                    (recur lst env))
              'if (if (EVAL a1 env)
                    (recur a2 env)
                    (when (> (count ast) 3)
                      (recur (nth ast 3) env)))
              'fn* (with-meta (fn [& args]
                                (EVAL a2 (env/env-new env a1 args)))
                     {:expr a2
                      :params a1
                      :env env
                      :is-macro false})
              'quote a1
              'quasiquote (recur (quasiquote a1) env)
              'quasiquoteexpand (quasiquote a1)
              'defmacro! (let [f (EVAL a2 env)]
                           (env/env-set! a1 (with-meta f
                                              (assoc (meta f) :is-macro true))
                                         env))
              'macroexpand (macroexpand a1 env)
              (let [new-ast (eval-ast ast env)
                    f (first new-ast)
                    args (rest new-ast)]
                (if-let [{:keys [expr env params]} (meta f)]
                  (recur expr (env/env-new env params args))
                  (apply f args)))))))
      (eval-ast ast env))))

(defn quasiquote-sequence [ast]
  (if-not (empty? ast)
    (let [elt (first ast)]
      (if (and (or (vector? elt)  (seq? elt)) (= 'splice-unquote (first elt)))
        (list 'concat (second elt) (quasiquote-sequence (rest ast)))
        (list 'cons (quasiquote elt) (quasiquote-sequence (rest ast)))))
    '()))

(defn quasiquote [ast]
  (cond
    (and (seq? ast) (= 'unquote (first ast))) (second ast)
    (seq? ast) (quasiquote-sequence ast)
    (vector? ast) (list 'vec (quasiquote-sequence ast))
    (or (map? ast) (symbol? ast)) (list 'quote ast)
    :else ast))

(defn is-macro-call [ast env]
  (and (seq? ast)
       (symbol? (first ast))
       (env/env-find (first ast) env)
       (:is-macro (meta (env/env-get (first ast) env)))))

(defn macroexpand [ast env]
  (loop [ast ast]
    (if (is-macro-call ast env)
      (let [macro (env/env-get (first ast) env)
            rst (rest ast)]
        (recur (apply macro (rest ast))))
      ast)))

(defn eval-ast [ast env]
  (cond
    (symbol? ast) (env/env-get ast env)
    (seq? ast) (mapv #(EVAL % env) ast)
    (vector? ast) (mapv #(EVAL % env) ast)
    (map? ast) (->> ast
                    (apply concat)    ; Flatten to vector
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

; Expose `EVAL` to mal through `eval`
(env/env-set! (symbol "eval") (fn [ast]
                                (EVAL ast repl-env)) repl-env)

; Define some functions in mal itself
(rep "(def! not (fn* (a) (if a false true)))")
(rep "(def! load-file (fn* [f] (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))")
(rep "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))")

(defn repl-loop []
  (print "user> ")
  (flush)
  (when-let [l (readline/readline "")]
    (try
      (println (rep l))
      (catch Throwable e (clojure.repl/pst e)))
    (recur)))

(defn -main [& args]
  (env/env-set! '*ARGV* (rest args) repl-env)
  (if args
    (rep (str "(load-file \"" (first args) "\" )"))
    (repl-loop)))
