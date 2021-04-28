(ns mal.printer
  (:require [clojure.string :as s]))

(defn pr-str [sexp]
  (cond
    (number? sexp) sexp
    (string? sexp) (clojure.core/pr-str sexp)
    (symbol? sexp) (str sexp)
    (nil? sexp) "nil"
    (seq? sexp) (str "(" (s/join " " (map pr-str sexp)) ")")
    (vector? sexp) (str "[" (s/join " " (map pr-str sexp)) "]")
    :else (str sexp)))
