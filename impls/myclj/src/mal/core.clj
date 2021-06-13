(ns mal.core
  (:refer-clojure :exclude [pr-str ns])
  (:require [mal.printer :as printer]
            [clojure.string :as s]
            [mal.reader :as reader]
            [mal.readline :as readline]))

(def ns
  {'= =
   'count count
   '< #(< %1 %2)
   '<= #(<= %1 %2)
   '> #(> %1 %2)
   '>= #(>= %1 %2)
   'pr-str (fn [& args] (s/join " " (map #(printer/pr-str %1 true) args)))
   'str (fn [& args] (s/join "" (map #(printer/pr-str %1 false) args)))
   'prn (fn [& args] (println (s/join " " (map #(printer/pr-str %1 true) args))))
   'println (fn [& args] (println (s/join " " (map #(printer/pr-str %1 false) args))))
   'read-string reader/read-str
   'slurp slurp
   'empty? empty?
   'list? seq?
   'list list
   'vector? vector?
   'vec vec
   'vector vector
   'atom atom
   'atom? printer/atom?
   'deref deref
   'reset! reset!
   'swap! swap!
   'cons cons
   'concat concat
   'nth nth
   'first first
   'rest rest
   'apply apply
   'map (fn [f l] (doall (map f l)))
   'throw (fn [v] (throw (ex-info "mal exception" {:value v})))
   'nil? nil?
   'true? true?
   'false? false?
   'symbol? symbol?
   'symbol symbol
   'keyword keyword
   'keyword? keyword?
   'sequential? sequential?
   'hash-map hash-map
   'map? map?
   'assoc assoc
   'dissoc dissoc
   'get get
   'contains? contains?
   'keys (fn [map] (let [ks (keys map)] (if (nil? ks) '() ks)))
   'vals (fn [map] (let [vs (vals map)] (if (nil? vs) '() vs)))
   'readline (fn [prompt]
               (print prompt)
               (flush)
               (readline/readline ""))
   'time-ms (fn [] (System/currentTimeMillis))
   'meta (fn [o] (:mal-meta (meta o)))
   'with-meta (fn [o m] (let [old-meta (meta o)
                              new-meta (assoc old-meta :mal-meta m)]
                          (with-meta o new-meta)))
   'fn? (fn [o] (if (and (fn? o) (not (:is-macro (meta o)))) true false))
   'macro? (fn [o] (if (and (fn? o) (:is-macro (meta o))) true false))
   'string? string?
   'number? number?
   'seq (fn [s]  (seq (if (string? s)
                        (map str s)
                        s)))
   'conj conj
   'clojure-eval (fn [s] (println (eval (read-string s))))})
