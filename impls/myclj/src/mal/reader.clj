(ns mal.reader
  (:require [clojure.string :as s]))

(defn reader
  "Create a stateful reader from a sequence of matches"
  [tokens]
  {:tokens tokens :cursor (atom 0)})

(defn rdr-peek
  "Look at the token at the cursor without popping it"
  [r]
  (nth (:tokens r) @(:cursor r)))

(defn rdr-next
  "Look at the token at the cursor and pop it"
  [r]
  (nth (:tokens r) (dec (swap! (:cursor r) inc))))

(defn rdr-pop
  "Remove the first token and return the reader"
  [r]
  (rdr-next r)
  r)

(defn rdr-empty?
  [r]
  (>= @(:cursor r) (count (:tokens r))))

(defn tokenize [s]
  (->> s
       (re-seq
        #"[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:\\.|[^\\\"])*\"?|;.*|[^\s\[\]{}('\"`,;)]*)")
       (map second)
       (filter (complement empty?))
       reader))

(declare read-atom)
(declare read-form)
(declare read-list)
(declare read-vec)

(defn unescape [s]
  (-> s
      ;; We have to 'hide' escaped slashes from the unescaper
      (s/replace "\\\\" (-> 0x1F4A9 (Character/toChars) String.))
      (s/replace "\\n" "\n")
      (s/replace "\\\"" "\"")
      (s/replace (-> 0x1F4A9 (Character/toChars) String.) "\\")))

(def number-re #"^-?[0-9]+(\.[0-9]+)?$")
(def invalid-str-re #"^\"([^\"]*)$")
(def str-re #"^\"(([^\\\"]|[\\].)*)\"$")

(defn read-atom [r]
  (let [tok (rdr-next r)]
    (cond
      (= \: (nth tok 0)) (keyword (subs tok 1))
      (= "true" tok) true
      (= "false" tok) false
      (= "nil" tok) nil
      (re-seq number-re tok) (read-string tok)
      (re-seq str-re tok) (->> tok (re-find str-re) second unescape)
      (re-seq invalid-str-re tok) (throw (Exception. "expected closing \", EOF"))
      :else (symbol tok))))

(defn read-form [r]
  (condp = (rdr-peek r)
    "(" (read-list (rdr-pop r))
    "[" (read-vec (rdr-pop r))
    (read-atom r)))

(defn read-vec [r]
  (loop [l []]
    (if (rdr-empty? r)
      (throw (Exception. "expected ')', got EOF"))
      (if (= (rdr-peek r) "]")
        (do (rdr-pop r) l)
        (recur (conj l (read-form r)))))))

(defn read-list [r]
  (loop [l '()]
    (if (rdr-empty? r)
      (throw (Exception. "expected ')', got EOF"))
      (if (= (rdr-peek r) ")")
        (do (rdr-pop r) l)
        (recur (concat l [(read-form r)]))))))

(defn read-str [s]
  (-> s
      tokenize
      read-form))
