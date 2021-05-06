(ns mal.printer
  (:require [clojure.string :as s])
  (:require [clojure.test :refer [function?]]))

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
     :else (str sexp))))
