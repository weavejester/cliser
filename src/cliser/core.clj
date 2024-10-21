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

(defn register [form]
  (let [form-id (fingerprint form)]
    (swap! registry assoc form-id form)
    form-id))

(defn lookup [form-id]
  (@registry form-id))

(defprotocol Endpoint
  (execute-on [endpoint env form-id]))

(defrecord LocalEndpoint []
  Endpoint
  (execute-on [_ env form-id]
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
