(ns malli.impl.ref
  (:require [malli.core :as m]
            [malli.registry :as mr]))

(defn -identify-ref-schema [schema]
  {:scope (-> schema m/-options m/-registry mr/-schemas)
   :name (m/-ref schema)})
