(ns cliser.core
  (:require [clj-commons.digest :as digest]
            [clojure.walk :as walk]))

(defn- sorted [data]
  (walk/postwalk #(cond
                    (map? %) (apply sorted-map (apply concat %))
                    (set? %) (apply sorted-set %)
                    :else    %)
                 data))

(defn- fingerprint [data]
  (digest/sha-256 (pr-str (sorted data))))

(def ^:private registry (atom {}))

(defn register [bindings form]
  (let [id (fingerprint form)
        f  (eval `(fn [{:syms [~@bindings]}] ~@form))]
    (swap! registry assoc id f)
    id))

(defn lookup [form-id]
  (@registry form-id))

(defprotocol Endpoint
  (execute-on [endpoint env form-id]))

(defrecord LocalEndpoint []
  Endpoint
  (execute-on [_ env id]
    ((lookup id) env)))

(def local-endpoint ->LocalEndpoint)

(defn- find-symbols [form]
  (into #{} (filter symbol?) (tree-seq coll? seq form)))

(defn- local-env [env form]
  (let [symbols (filter (find-symbols form) (keys env))]
    (reduce #(assoc %1 `(quote ~%2) %2) {} symbols)))

(defmacro with-endpoint [endpoint & body]
  (let [env (local-env &env body)
        id  (register (vals env) body)]
    `(execute-on ~endpoint ~env (quote ~id))))
