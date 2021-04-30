(ns mal.env)

(defn env-new
  ([] (env-new nil))
  ([outer] (atom {:outer outer :data {}})))

(defn env-set! [k v env]
  (swap! env assoc-in [:data k] v)
  v)

(defn env-find [k env]
  (when env
    (if (contains? (:data @env) k)
      env
      (recur k (:outer @env)))))

(defn env-get [k env]
  (if-let [env (env-find k env)]
    (get-in @env [:data k])
    (throw (Exception. (str "Symbol '" k "' not found")))))
