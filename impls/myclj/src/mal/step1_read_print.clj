(ns mal.step1-read-print
  (:require [clojure.repl]
            [mal.readline :as readline]
            [mal.reader :as reader]
            [mal.printer :as printer])
  (:gen-class))

(defn READ [s]
  (reader/read-str s))

(defn EVAL [arg]
  arg)

(defn PRINT [arg]
  (printer/pr-str arg))

(defn rep [arg]
  (-> arg
      READ
      EVAL
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
