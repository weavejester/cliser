(ns cliser.core)

(def ^:private registry (atom {}))

(defn register [form]
  (let [form-id (gensym)]
    (swap! registry assoc form-id form)
    form-id))

(defn lookup [form-id]
  (@registry form-id))

(defprotocol Endpoint
  (execute-on [endpoint env form-id]))

(defrecord LocalEndpoint []
  Endpoint
  (execute-on [_ env form-id]
    (prn {:env env, :form-id form-id})
    (eval `(let ~(reduce-kv conj [] env) ~@(lookup form-id)))))

(def local-endpoint ->LocalEndpoint)

(defn- find-symbols [form]
  (into #{} (filter symbol?) (tree-seq coll? seq form)))

(defn- local-env [env form]
  (let [symbols (filter (find-symbols form) (keys env))]
    (reduce #(assoc %1 `(quote ~%2) %2) {} symbols)))

(defmacro with-endpoint [endpoint & body]
  (let [form-id (register body)]
    `(execute-on ~endpoint ~(local-env &env body) (quote ~form-id))))
