(ns mal.printer
  (:refer-clojure :exclude [pr-str])
  (:require [clojure.string :as s]
            [clojure.test :refer [function?]]))

(defn atom? [v] (= (type v) clojure.lang.Atom))

(defn escape
  [s]
  (-> s
      (s/replace "\\" "\\\\")
      (s/replace "\"" "\\\"")
      (s/replace "\n" "\\n")))

(defn pr-str
  ([sexp] (pr-str sexp true))
  ([sexp print_readably]
   (cond
     (number? sexp) sexp
     (string? sexp) (if print_readably
                      (str "\"" (escape sexp) "\"")
                      sexp)
     (symbol? sexp) (str sexp)
     (nil? sexp) "nil"
     (seq? sexp) (str "(" (s/join " " (map #(pr-str % print_readably) sexp)) ")")
     (vector? sexp) (str "[" (s/join " " (map #(pr-str % print_readably) sexp)) "]")
     (function? sexp) "#<function>"
     (atom? sexp) (str "(atom " (pr-str @sexp) ")")
     (map? sexp) (str "{" (s/join " " (map (fn [[k v]]
                                             (str (pr-str k print_readably)
                                                  " "
                                                  (pr-str v print_readably))) sexp)) "}")
     :else (str sexp))))
