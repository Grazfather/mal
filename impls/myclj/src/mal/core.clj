(ns mal.core
  (:require [mal.printer :as printer]
            [clojure.string :as s]
            [mal.reader :as reader]))

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
   'vals (fn [map] (let [vs (vals map)] (if (nil? vs) '() vs)))})
