(ns mal.step0-repl
  (:require [mal.readline :as readline])
  (:gen-class))

(defn READ [arg]
  arg)

(defn EVAL [arg]
  arg)

(defn PRINT [arg]
  arg)

(defn rep [arg]
  (-> arg
      READ
      EVAL
      PRINT))

(defn repl-loop []
  (print "user> ")
  (flush)
  (when-let [l (readline/readline "")]
    (println (rep l))
    (recur)))


(defn -main [& args]
  (repl-loop))
