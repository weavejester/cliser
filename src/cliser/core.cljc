(ns cliser.core
  (:require [clj-commons.digest :as digest]
            [clojure.core.async :as a]
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

(defn- register [symbols form]
  (let [form-id (fingerprint form)]
    (swap! form-registry assoc form-id [symbols form])
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

(defmacro on-endpoint [endpoint & body]
  (let [symbols (filter (find-symbols body) (keys &env))
        form-id (register symbols body)]
    `(do
       ;; this function does nothing at runtime, but ensures that the macros
       ;; in its body will be evaluated at compile time
       ~(->function symbols body)
       (execute-on ~endpoint ~(binding-map symbols) ~form-id))))

(defmacro with-endpoints [bindings & body]
  `(let ~bindings
     (a/go ~@(let [bindmap (apply hash-map bindings)]
               (walk/postwalk
                #(if (and (seq? %) (contains? bindmap (first %)))
                   `(a/<! (on-endpoint ~@%))
                   %)
                body)))))

(defn- form-functions [registry]
  (reduce-kv (fn [m k [syms body]]
               (assoc m k (->function syms `((a/go ~@body)))))
             {} registry))

(defmacro compile-endpoints []
  `(reset! fn-registry ~(form-functions @form-registry)))
