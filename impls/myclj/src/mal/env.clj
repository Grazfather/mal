(ns  mal.env)

(defn env-set! [k v env]
  (swap! env assoc-in [:data k] v)
  v)

(defn env-new
  ([] (env-new nil))
  ([outer] (env-new outer [] []))
  ([outer binds exprs]
   (let [env (atom {:outer outer :data {}})]
     (loop [bind (first binds)
            rbinds (rest binds)
            expr (first exprs)
            rexprs (rest exprs)]
       (when bind
         (if (= bind (symbol "&")) ;; Variadic arg
           (env-set! (first rbinds) (if-not (nil? expr) (cons expr rexprs) '()) env)
           (do
             (env-set! bind expr env)
             (recur (first rbinds) (rest rbinds) (first rexprs) (rest rexprs))))))
     env)))

(defn env-find [k env]
  (when env
    (if (contains? (:data @env) k)
      env
      (recur k (:outer @env)))))

(defn env-get [k env]
  (if-let [env (env-find k env)]
    (get-in @env [:data k])
    (throw (Exception. (str "'" k "' not found")))))
