;; demo (not included in jar)
(ns malli.optimize
  (:require [malli.core :as m]
            [malli.impl.util :as miu]
            [malli.solver :as solver]))

(set! *warn-on-reflection* true)
(declare -solutions-validator)

(defmulti -solution-validator (fn [solution options] (:type solution)))
(defn- -min-max-validator [{:keys [min-number max-number >-number <-number]}]
  (when (or min-number max-number >-number <-number)
    (miu/-every-pred
      (into (if (and min-number (= min-number max-number))
              [#(= min-number %)]
              (cond-> []
                min-number (conj #(<= min-number %))
                >-number (conj #(< >-number %))
                max-number (conj #(<= % max-number))
                <-number (conj #(< % <-number))))))))
(defmethod -solution-validator :number
  [solution _]
  (let [mmv (-min-max-validator solution)]
    (miu/-every-pred
      (cond-> [number?]
        mmv (conj mmv)))))
(defmethod -solution-validator :int
  [solution _]
  (let [mmv (-min-max-validator solution)]
    (miu/-every-pred
      (cond-> [int?]
        mmv (conj mmv)))))

(defn- -validate-map-linearly
  [{keys-solutions :keys vals-solutions :vals get-solutions :get
    :keys [keyset open-map min-count max-count default-keys default-vals]} options]
  (let [required-keys (not-empty (into #{} (mapcat (fn [[k v]] (when (= :present v) [k]))) keyset))
        nrequired (count required-keys)
        forbidden-keys (let [phs (into #{} (mapcat (fn [[k v]] (when (= :absent v) [k]))) keyset)]
                         #?(:clj (java.util.HashSet. ^java.util.Set phs)
                            :default phs))
        valid-key? (-solutions-validator (or keys-solutions [{}]) options)
        valid-val? (-solutions-validator (or vals-solutions [{}]) options)
        error-val (reduced -1)
        default-validator (fn [nrequired k v]
                            (if (#?(:clj .contains :default contains?) forbidden-keys k)
                              error-val
                              (if (valid-key? k)
                                (if (valid-val? v)
                                  nrequired
                                  error-val)
                                (if open-map
                                  ;;can't have default and be open but if you could this would pass
                                  nrequired
                                  error-val))))
        get-validators (let [phm (into {} (map (fn [[k s]]
                                                 (let [valid? (-solution-validator s options)
                                                       register-required (if (contains? required-keys k) dec identity)]
                                                   [k (fn [nrequired _ v]
                                                        (if (valid? v)
                                                          (register-required nrequired)
                                                          error-val))])))
                                       get-solutions)]
                         #?(:clj (java.util.HashMap. ^java.util.Map phm)
                            :default phm))]
    (miu/-every-pred
      (-> [map?]
          (cond->
            (or min-count max-count) (conj (comp (-min-max-validator {:min-number min-count :max-number max-count}) count)))
          (conj #(zero? (reduce-kv (fn [state k v]
                                     ((#?(:clj .getOrDefault) get-validators k default-validator) state k v))
                                   nrequired %)))))))

(defn- -validate-map-via-lookup
  [{keys-solutions :keys vals-solutions :vals get-solutions :get
    :keys [keyset open-map min-count max-count default-keys default-vals]} options]
  (assert nil "TODO"))

;;hmm unclear which validator to use between keys vs default-keys
(defmethod -solution-validator :map
  [{:keys [open-map] :as solution} options]
  (if true #_open-map
    (-validate-map-linearly solution options)
    (-validate-map-via-lookup solution options)))

(defmethod -solution-validator nil [_ _] any?)

(defn -solutions-validator [solutions options]
  (let [;;TODO group related types together, maybe change defmulti to accept multiple solutions?
        ;; could also use something smarter than -some-pred?
        ors (mapv #(-solution-validator % options) solutions)]
    (miu/-some-pred ors)))

(defn validator
  ([?schema] (validator ?schema nil))
  ([?schema options]
   (-> ?schema
       (solver/solve options)
       (-solutions-validator options))))

(defn validate
  ([?schema value]
   (validate ?schema value nil))
  ([?schema value options]
   ((validator ?schema options) value)))

(defmulti -solution-explainer (fn [solution path options] (:type solution)))
(defn- -simple-solutions-explainer [solutions path options]
  (let [validator (-solutions-validator solutions options)
        ;;TODO :schema
        this nil]
    (fn explain [x in acc]
      (if-not (validator x) (conj acc (miu/-error path in this x)) acc))))
(defmethod -solution-explainer :int [solution path options] (-simple-solutions-explainer [solution] path options))

(defn -solutions-explainer [solutions path options]
  (let [;;TODO group related types together, maybe change defmulti to accept multiple solutions?
        ;; could also use something smarter than -some-pred?
        ors (mapv #(-solution-explainer % path options) solutions)]
    (miu/-some-pred ors)))

(defn explainer
  ([?schema] (explainer ?schema nil))
  ([?schema options]
   (let [schema (m/schema ?schema options)
         solutions (solver/solve schema options)
         explainer' (m/-cached schema ::explainer #(-> %
                                                       (solver/solve options)
                                                       (-solutions-explainer [] options)))]
     (fn explainer
       ([value] (explainer value [] []))
       ([value in acc]
        (when-let [errors (seq (explainer' value in acc))]
          {:schema schema
           :value value
           :errors errors}))))))

(defn explain
  ([?schema value] (explain ?schema value nil))
  ([?schema value options] ((explainer ?schema options) value [] [])))
