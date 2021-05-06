(ns mal.core
  (:require [mal.printer :as printer]
            [clojure.string :as s]))

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
   'empty? empty?
   'list? seq?
   'list list})
