(ns cliser.core)

(defprotocol Endpoint
  (execute-on [endpoint env form]))

(defrecord LocalEndpoint []
  Endpoint
  (execute-on [_ env form]
    (eval `(let ~(reduce-kv conj [] env) ~@form))))

(def local-endpoint ->LocalEndpoint)

(defn- find-symbols [form]
  (into #{} (filter symbol?) (tree-seq coll? seq form)))

(defn- local-env [env form]
  (let [symbols (filter (find-symbols form) (keys env))]
    (reduce #(assoc %1 `(quote ~%2) %2) {} symbols)))

(defmacro with-endpoint [endpoint & body]
  `(execute-on ~endpoint ~(local-env &env body) (quote ~body)))
