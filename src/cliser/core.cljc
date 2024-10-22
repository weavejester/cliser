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

(defn- ->function [symbols body]
  `(fn [{:syms [~@symbols]}] ~@body))

(def ^:private form-registry (atom {}))
(def ^:private fn-registry   (atom {}))

(defn- register [symbols body]
  (let [form    (->function symbols body)
        form-id (fingerprint form)]
    (swap! form-registry assoc form-id form)
    form-id))

(defprotocol Endpoint
  (execute-on [endpoint env id]))

(defrecord LocalEndpoint []
  Endpoint
  (execute-on [_ env id] ((@fn-registry id) env)))

(def local-endpoint ->LocalEndpoint)

(defn- find-symbols [form]
  (into #{} (filter symbol?) (tree-seq coll? seq form)))

(defn- binding-map [symbols]
  (reduce #(assoc %1 `(quote ~%2) %2) {} symbols))

(defmacro with-endpoint [endpoint & body]
  (let [symbols (filter (find-symbols body) (keys &env))
        form-id (register symbols body)]
    `(do ~(->function symbols body)  ;; force macro evaluation
         (execute-on ~endpoint ~(binding-map symbols) ~form-id))))

(defmacro compile-endpoints []
  `(reset! fn-registry ~(deref form-registry)))
