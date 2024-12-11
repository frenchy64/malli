;; See also `malli.generator-ast` for viewing generators as data
(ns malli.generator
  (:require [clojure.math :as math]
            [clojure.set :as set]
            [clojure.spec.gen.alpha :as ga]
            [clojure.string :as str]
            [clojure.test.check :as check]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.random :as random]
            [clojure.test.check.rose-tree :as rose]
            [malli.solver :as solver]
            [malli.core :as m]
            [malli.registry :as mr]
            [malli.util :as mu]
            [malli.impl.util :refer [-last -merge]]
            #?(:clj [borkdude.dynaload :as dynaload])))

(declare generator generate -create ^:private gen-one-of gen-double)

(defprotocol Generator
  (-generator [this options] "returns generator for schema"))

;;
;; generators
;;


;; # Notes for implementors
;;
;; For the most part, -schema-generator is a pretty direct translation from schemas to generators.
;; However, the naive implementation of recursive ref's (creating a generator for the dereferenced ref
;; and cutting off the generation at a certain depth) tends to create exponentially large test cases.
;;
;; We use a more sophisticated algorithm to achieve linearly sized test cases with recursive refs.
;; The next section describes the strategy implementors should use to participate in this improved behavior.
;; The theory behind this strategy is described in the section below ("Approach for recursive generators").
;;
;; ## Implementation strategy
;;
;; Say you have a composite schema you want to generate values for. You should extend `-schema-generator` and
;; call `generator` recursively on the `m/children`. Now, for every child generator, you need to consider the case
;; that the child generator generates no values, and how this might change the final generator.
;;
;; Use `-unreachable-gen?` to test whether your child generator generates no values (we'll call this an "unreachable" schema/generator).
;; If your parent generator cannot generate values, use `-never-gen` to return an unreachable generator.
;; 
;; Here are a few examples---compare them with the logic in their respective -schema-generator methods:
;;   [:maybe M] would generate like :nil if M were unreachable.
;;   [:map [:a M]] would itself be unreachable if M were unreachable.
;;   [:map [:a {:optional true} M]] would generate like [:map] if M were unreachable.
;;   [:vector M] would generate like [:= []] if M were unreachable.
;;   [:vector {:min 1} M] would itself be unreachable if M were unreachable.

(def nil-gen (gen/return nil))

(defn- -child [schema options] (first (m/children schema options)))
(defn- -child-gen [schema options] (generator (-child schema options) options))

(defn -never-gen
  "Return a generator of no values that is compatible with -unreachable-gen?."
  [{::keys [original-generator-schema] :as _options}]
  (with-meta (gen/sized (fn [_]
                          (m/-fail! ::unsatisfiable-schema
                                    (cond-> {}
                                      original-generator-schema (assoc :schema original-generator-schema)))))
             {::never-gen true
              ::original-generator-schema original-generator-schema}))

(defn -unreachable-gen?
  "Returns true iff generator g generators no values."
  [g] (-> (meta g) ::never-gen boolean))

(defn -not-unreachable [g] (when-not (-unreachable-gen? g) g))
(defn -unreachable [g] (when (-unreachable-gen? g) g))

(defn- -random [seed] (if seed (random/make-random seed) (random/make-random)))

(defn -min-max [schema options]
  (let [{:keys [min max] gen-min :gen/min gen-max :gen/max} (m/properties schema options)]
    (when (and min gen-min (< gen-min min))
      (m/-fail! ::invalid-property {:key :gen/min, :value gen-min, :min min}))
    (when (and max gen-max (> gen-max max))
      (m/-fail! ::invalid-property {:key :gen/max, :value gen-min, :max min}))
    {:min (or gen-min min)
     :max (or gen-max max)}))

(defn- -solve-schema [schema options]
  (solver/solve schema (assoc options ::solver/mode :gen ::solver/allow-incomplete-solution true)))

(defn- -solve-each [f {::keys [solutions] :as options}]
  (gen-one-of (into [] (keep #(f % (dissoc options ::solutions))) (or solutions [{}])) options))

(def ^:private -max-double #?(:clj Double/MAX_VALUE :cljs (.-MAX_VALUE js/Number)))
(def ^:private -min-double (- -max-double))
(def ^:private -max-long #?(:clj Long/MAX_VALUE :cljs (dec (apply * (repeat 53 2)))))
(def ^:private -min-long #?(:clj Long/MIN_VALUE :cljs (- -max-long)))

(defn- -with-number-bounds [goptions {:keys [min-number max-number >-number <-number] :as solution}]
  (cond-> goptions
    solution (cond->
               min-number (update :min #(if %
                                          (max % min-number)
                                          min-number))
               >-number (update :min #(if %
                                        (if (<= % >-number)
                                          (math/next-up >-number)
                                          %)
                                        (math/next-up >-number)))
               max-number (update :max #(if %
                                          (min % max-number)
                                          max-number))
               <-number (update :min #(if %
                                        (if (<= <-number %)
                                          (math/next-down <-number)
                                          %)
                                        (math/next-down <-number))))))

(defn- -finite-bounds-between? [min max lb ub]
  (and (or (not min) (and (not (infinite? min))
                          (<= lb min)))
       (or (not max) (and (not (infinite? max))
                          (<= max ub)))
       (or (not (and min max)) (<= min max))))

(defn- -reachable-double*-options [{:keys [min max] :as goptions}]
  (when (-finite-bounds-between? min max -min-double -max-double)
    (-> goptions
        (update :infinite? #(if (some? %) % false))
        (update :NaN? #(if (some? %) % false)))))

(defn- -double-gen* [goptions options]
  (-solve-each (fn [solution options]
                 (some-> (or goptions {}) (-with-number-bounds solution) -reachable-double*-options gen/double*))
               options))

(defn- -reachable-large-integer*-options [{:keys [min max] :as goptions}]
  (prn "-reachable-large-integer*-options" min max)
  (when (-finite-bounds-between? min max -min-long -max-long)
    (let [imin (some-> min math/ceil long)
          imax (some-> max math/floor long)]
      (prn "yes" imin imax)
      (when (-finite-bounds-between? imin imax min max)
        (prn "finite yes" imin imax)
        (cond-> (or goptions {})
          imin (assoc :min imin)
          imax (assoc :max imax))))))

(defn- -int-gen* [goptions options]
  (-solve-each (fn [solution options]
                 (prn "-int-gen* solution" solution)
                 (some-> (or goptions {}) (-with-number-bounds solution) -reachable-large-integer*-options gen/large-integer*))
               options))

(defn- -number-gen* [goptions options]
  (gen-one-of [(-int-gen* goptions options)
               (-double-gen* goptions options)]
              options))

(defn- gen-fmap [f gen] (or (-unreachable gen) (gen/fmap f gen)))
(defn- gen-fcat [gen] (gen-fmap #(apply concat %) gen))
(defn- gen-tuple [gens] (or (some -unreachable gens) (apply gen/tuple gens)))
(defn- gen-maybe [g] (if (-unreachable-gen? g) nil-gen (gen/one-of [nil-gen g])))
(def ^:private double-default {:infinite? false, :NaN? false})
(defn- gen-double [opts] (gen/double* (-> (into double-default opts) (update :min #(some-> % double)) (update :max #(some-> % double)))))

(defn- gen-vector [{:keys [min max]} g]
  (cond
    (-unreachable-gen? g) (if (zero? (or min 0)) (gen/return []) g)
    (and min (= min max)) (gen/vector g min)
    (and min max) (gen/vector g min max)
    min (vary-meta (gen/sized #(gen/vector g min (+ min %))) assoc ::generator-ast {:op :vector-min :generator g :min min})
    max (gen/vector g 0 max)
    :else (gen/vector g)))

(defn- gen-vector-distinct-by [schema {:keys [min] :as m} f g]
  (if (-unreachable-gen? g)
    (if (= 0 (or min 0)) (gen/return []) g)
    (gen/vector-distinct-by f g (-> (assoc (if (and min (= min max))
                                             {:num-elements min}
                                             (set/rename-keys m {:min :min-elements :max :max-elements}))
                                           :ex-fn #(m/-exception ::distinct-generator-failure (assoc % :schema schema)))))))

(defn- -intersect-solutions [solutions options]
  (update options ::solutions (fn [solutions']
                                (solver/-intersect [(or solutions' [{}]) solutions]))))

(defn- -string-gen [schema options]
  (->> options
       (-solve-each (fn [{min :min-count max :max-count :as solution} options]
                      (gen-fmap str/join (gen-vector {:min min :max max} gen/char-alphanumeric))))))

(defn- -coerce-coll-result [solution]
  (case (:type solution)
    (nil :seqable :coll :counted :indexed :vector :sequential) identity
    :eduction eduction
    :object-array #(into-array #?(:clj Object) %)
    :list #(apply list %)
    (:int :double :number :map :set) nil
    (do (println "Unknown regex result solution type" (:type solution))
        identity)))

(defn- -coll-gen
  ([schema options] (-coll-gen schema identity options))
  ([schema f options]
   (->> options
        (-solve-each (fn [{min :min-count max :max-count :as solution} options]
                       (when-some [f (-coerce-coll-result solution)]
                         (gen-fmap f (gen-vector {:min min :max max} (-child-gen schema options)))))))))

(defn- -coerce-coll-distinct-result [solution]
  (case (:type solution)
    (nil :seqable :coll :counted :indexed :vector :sequential) identity
    :eduction eduction
    :object-array #(into-array #?(:clj Object) %)
    :list #(apply list %)
    :map #(into {} (map vec) %)
    :set set
    (:int :double :number) nil
    (do (println "Unknown regex result solution type" (:type solution))
        identity)))

(defn- gen-vector-distinct [schema m g] (gen-vector-distinct-by schema m identity g))

(defn- -coll-distinct-gen [schema f options]
  (->> options
       (-solve-each (fn [{min :min-count max :max-count :as solution} options]
                      (when-some [f (-coerce-coll-distinct-result solution)]
                        (gen-fmap f (gen-vector-distinct schema {:min min :max max} (-child-gen schema options))))))))

(defn -and-gen [schema options]
  (let [[gchild & schildren] (m/children schema)
        solutions (let [options (assoc options ::solver/mode :gen)]
                    (some-> (seq (cond-> (map #(solver/solve % options) schildren)
                                   (::solutions options) (conj (::solutions options))))
                            solver/-intersect))]
    (if-some [gen (-not-unreachable (generator gchild (assoc options ::solutions solutions)))]
      (gen/such-that (m/validator schema options) gen
                     {:max-tries 100
                      :ex-fn #(m/-exception ::and-generator-failure
                                            (assoc % :schema schema))})
      (-never-gen options))))

(defn- gen-one-of [options gs]
  (if-some [gs (not-empty (into [] (keep -not-unreachable) gs))]
    (if (= 1 (count gs)) (nth gs 0) (gen/one-of gs))
    (-never-gen options)))

(defn- -seqable-gen [schema options]
  (->> options
       (-solve-each (fn [{min :min-count :as solution} options]
                      (let [el (-child schema options)
                            type->options #(-intersect-solutions [{:type %}] (assoc options ::solutions [solution]))]
                        (gen-one-of
                          options
                          (-> []
                              (cond->
                                (or (nil? min) (zero? min))
                                (conj nil-gen))
                              (into (map #(-coll-gen schema % options))
                                    [identity vec eduction #(into-array #?(:clj Object) %)])
                              (conj (-coll-distinct-gen schema set options))
                              (cond->
                                (and (= :tuple (m/type el))
                                     (= 2 (count (m/children el))))
                                (conj (let [[k v] (m/children el)]
                                        (generator [:map-of (or (m/properties schema) {}) k v] options)))))))))))

(defn- ->such-that-opts [schema] {:max-tries 100 :ex-fn #(m/-exception ::such-that-failure (assoc % :schema schema))})
(defn- gen-such-that [schema pred gen] (or (-unreachable gen) (gen/such-that pred gen (->such-that-opts schema))))

(defn -and-gen [schema options]
  (gen-such-that schema (m/validator schema options) (-child-gen schema options)))

(defn- gen-one-of [options gs]
  (if-some [gs (not-empty (into [] (keep -not-unreachable) gs))]
    (if (= 1 (count gs)) (nth gs 0) (gen/one-of gs))
    (-never-gen options)))

(defn -or-gen [schema options]
  (gen-one-of options (map #(generator % options) (m/children schema options))))

(defn- -merge-keyword-dispatch-map-into-entries [schema]
  (let [dispatch (-> schema m/properties :dispatch)]
    (cond-> schema
      (keyword? dispatch)
      (mu/transform-entries
       #(map (fn [[k :as e]]
               (cond-> e
                 (not= ::m/default k)
                 (update 2 mu/merge [:map [dispatch [:= nil k]]]))) %)
       (m/options schema)))))

(defn -multi-gen [schema options]
  (gen-one-of options (map #(generator (last %) options) (m/entries (-merge-keyword-dispatch-map-into-entries schema) options))))

(defn- -build-map [kvs]
  (persistent!
   (reduce
    (fn [acc [k v]]
      (cond (and (= k ::m/default) (map? v)) (reduce-kv assoc! acc v)
            (nil? k) acc
            :else (assoc! acc k v)))
    (transient {}) kvs)))

(defn- -entry-gen [[k s] options]
  (cond->> (gen-fmap #(do [k %]) (generator s options)) (-> s m/properties :optional) gen-maybe))

(defn- -value-gen [k s options]
  (let [g (generator s options)]
    (cond->> g (-not-unreachable g) (gen/fmap (fn [v] [k v])))))

(defn -map-gen [schema options]
  (->> options
       (-solve-each (fn [{get-solutions :get :keys [keyset] :as solutions} options]
                      ;(->> schema m/entries (map #(-entry-gen % options)) gen-tuple (gen-fmap -build-map))
                      (loop [[[k s :as e] & entries] (m/entries schema)
                             gens []]
                        (if (nil? e)
                          (gen/fmap -build-map (apply gen/tuple gens))
                          (let [vgen #(-value-gen k s (assoc options ::solutions (get-solutions k)))]
                            (case (keyset k)
                              :optional
                              (recur entries (conj gens (gen-one-of [nil-gen (vgen)] options)))
                              :present
                              (if-some [g (-not-unreachable (vgen))]
                                (recur entries (conj gens g))
                                (-never-gen options))
                              :absent
                              (recur entries (conj gens nil-gen))))))))))

(defn -map-of-gen [schema options]
  (->> options
       (-solve-each (fn [{min :min-count max :max-count :as solution} options]
                      (let [[k-gen v-gen :as gs] (map #(generator % options) (m/children schema options))]
                        (->> (gen-tuple (map #(generator % options) (m/children schema options)))
                             (gen-vector-distinct-by schema {:min min :max max} #(nth % 0))
                             (gen-fmap #(into {} %))))))))

#?(:clj
   (defn -re-gen [schema options]
     ;; [com.gfredericks/test.chuck "0.2.10"+]
     (if-let [string-from-regex @(dynaload/dynaload 'com.gfredericks.test.chuck.generators/string-from-regex {:default nil})]
       (let [re (or (first (m/children schema options)) (m/form schema options))]
         (string-from-regex (re-pattern (str/replace (str re) #"^\^?(.*?)(\$?)$" "$1"))))
       (m/-fail! :test-chuck-not-available))))

;; # Approach for recursive generators
;;
;; `-ref-gen` is the only place where recursive generators can be created, and we use `gen/recursive-gen`
;; to handle the recursion. The challenge is that gen/recursive-gen requires _two_ arguments: the base
;; case (scalar gen) and the recursive case (container gen). We need to automatically split the schema argument into
;; these two cases.
;;
;; The main insight we use is that a base case for the schema cannot contain recursive references to itself.
;; A particularly useful base case is simply to "delete" all recursive references. To simulate this, we have the concept of
;; an "unreachable" generator, which represents a "deleted" recursive reference.
;;
;; For infinitely expanding schemas, this will return an unreachable generator--when the base case generator is used,
;; the error message in `-never-gen` will advise users that their schema is infinite.
;; 
;; 
;; Examples of base cases of some recursive schemas:
;;
;; Schema:    [:schema {:registry {::cons [:maybe [:vector [:tuple pos-int? [:ref ::cons]]]]}} ::cons]
;; Base case: [:schema {:registry {::cons [:nil                                            ]}} ::cons]
;;
;; Schema:    [:schema
;;             {:registry {::ping [:tuple [:= "ping"] [:maybe [:ref ::pong]]]
;;                         ::pong [:tuple [:= "pong"] [:maybe [:ref ::ping]]]}}
;;             ::ping]
;; Base case: [:schema
;;             {:registry {::ping [:tuple [:= "ping"] [:maybe [:ref ::pong]]]
;;                         ::pong [:tuple [:= "pong"] :nil                  ]}}
;;             ::ping]
;;
;; Once we have the base case, we first need determine if the schema is recursive---it's recursive
;; if more than one recursive reference was successfully "deleted" while creating the base case (see below for how we determine recursive references).
;; We can then construct the recursive case by providing `gen/recursive-gen` the base case
;; (this is why this particular base case is so useful) and then propagate the (smaller) generator
;; supplied by `gen/recursive-gen` to convert recursive references.

;; ## Identifying schema recursion
;; 
;; Refs are uniquely identified by their paired name and scope. If we see a ref with the
;; same name and scope as another ref we've dereferenced previously, we know that this is a recursion
;; point back to the previously seen ref. The rest of this section explains why.
;; 
;; Refs resolve via dynamic scope, which means its dereferenced value is the latest binding found
;; while expanding the schema until the point of finding the ref.
;; This makes the (runtime) scope at the ref's location part of a ref's identity---if the scope
;; is different, then it's (possibly) not the same ref because scope determines how schemas
;; transitively expand.
;;
;; To illustrate why a ref's name is an insufficient identifier, here is a schema that is equivalent to `[:= 42]`:
;; 
;;   [:schema {:registry {::a [:schema {:registry {::a [:= 42]}}
;;                             ;; (2)
;;                             [:ref ::a]]}}
;;    ;; (1)
;;    [:ref ::a]]
;;
;; If we identify refs just by name, we would have incorrectly detected (2) to be an (infinitely expanding) recursive
;; reference.
;;
;; In studying the previous example, we might think that since (1) and (2) deref to different schemas, it might sufficient to identify refs just by their derefs.
;; Unfortunately this just pushes the problem elsewhere.
;;
;; For example, here is another schema equivalent to `[:= 42]`:
;;
;;   [:schema {:registry {::a [:ref ::b] ;; (2)
;;                        ::b [:schema {:registry {::a [:ref ::b] ;; (4)
;;                                                 ::b [:= 42]}}
;;                             ;; (3)
;;                             [:ref ::a]]}}
;;    ;; (1)
;;    [:ref ::a]]
;;
;; If we identified ::a by its deref, it would look like (3) deref'ing to (4)
;; is a recursion point after witnessing (1) deref'ing to (2), since (2) == (4). Except this
;; is wrong since it's a different ::b at (2) and (4)! OTOH, if we identified (2) and (4) with their
;; dynamic scopes along with their form, they would be clearly different. Indeed, this
;; is another way to identify refs: pairing their derefs with their deref's scopes.
;; It is slightly more direct to use the ref's direct name and scope, which is why
;; we choose that identifier. The more general insight is that any schema is identified by its form+scope
;; (note: but only after trimming the scope of irrelevant bindings, see next pararaph).
;; That insight may be useful for detecting recursion at places other than refs.
;; 
;; Ref identifiers could be made smarter by trimming irrelevant entries in identifying scope.
;; Not all scope differences are relevant, so generators may expand more than strictly necessary
;; in the quest to find the "same" ref schema again. It could skip over refs that generate exactly the
;; same values, but their scopes are uninterestingly different (eg., unused bindings are different).
;;
;; For example, the following schema is recursive "in spirit" between (1) and (2), but since ::b
;; changes, the scope will differ, so the recursion will be detected between (2) and itself instead
;; (where the scope is constant):
;;
;;   [:schema {:registry {::a [:schema {:registry {::b :boolean}}
;;                             ;; (2)
;;                             [:or [:ref ::a] [:ref ::b]]]}}
;;    [:schema {:registry {::b :int}}
;;     ;; (1)
;;     [:or [:ref ::a] [:ref ::b]]]]

(defn- -identify-ref-schema [schema]
  {:scope (-> schema m/-options m/-registry mr/-schemas)
   :name (m/-ref schema)})

(defn -ref-gen [schema options]
  (let [ref-id (-identify-ref-schema schema)]
    (or (force (get-in options [::rec-gen ref-id]))
        (let [scalar-ref-gen (delay (-never-gen options))
              dschema (m/deref schema)]
          (cond->> (generator dschema (assoc-in options [::rec-gen ref-id] scalar-ref-gen))
            (realized? scalar-ref-gen) (gen/recursive-gen
                                        #(generator dschema (assoc-in options [::rec-gen ref-id] %))))))))

(defn -=>-gen [schema options]
  (let [output-generator (generator (:output (m/-function-info schema)) options)]
    (gen/return (m/-instrument {:schema schema} (fn [& _] (generate output-generator options))))))

(defn -function-gen [schema options]
  (gen/return (m/-instrument {:schema schema, :gen #(generate % options)} nil options)))

(defn -regex-generator [schema options]
  (cond-> (generator schema options) (not (m/-regex-op? schema)) (-> vector gen-tuple)))

(defn- entry->schema [e] (if (vector? e) (get e 2) e))
(defn- -re-entry-gen [e options] (-regex-generator (if (vector? e) (get e 2) e) options))

(defn- -coerce-regex-result [solution]
  (case (:type solution)
    (:counted :indexed :vector :sequential) vec
    (nil :coll :seq :seqable) sequence
    :list (comp #(do (assert (list? %)) %) #(apply list %))
    (:int :double :number :map :set) nil
    (do (println "Unknown regex result solution type" (:type solution))
        identity)))

(defn -cat-gen [schema options]
  (->> options
       (-solve-each (fn [solution options]
                      ;;(->> (m/children schema options) (map #(-re-entry-gen % options)) gen-tuple gen-fcat)
                      (let [gs (->> (m/children schema options)
                                    (map #(-regex-generator (entry->schema %) options)))]
                        (when (not-any? -unreachable-gen? gs)
                          (when-some [coerce (-coerce-regex-result solution)]
                            (->> gs
                                 (apply gen/tuple)
                                 (gen/fmap (comp coerce #(apply concat %)))))))))))

(defn -alt-gen [schema options]
  (->> (m/children schema options) (map #(-re-entry-gen % options)) (gen-one-of options)))

(defn -?-gen [schema options]
  (-solve-each (fn [solution options]
                 (let [child (-child schema options)]
                   (when-some [coerce (-coerce-regex-result solution)]
                     (if-some [g (-not-unreachable (generator child (cond-> options
                                                                      (m/-regex-op? child)
                                                                      (assoc ::solutions [solution]))))]
                       (if (m/-regex-op? child)
                         (gen/one-of [g (gen/return (coerce ()))])
                         (cond->> (gen/vector g 0 1)
                           (some-> (:type solution) (not= :vector))
                           (gen/fmap coerce)))
                       (gen/return (coerce ()))))))
               options))

(defn -*-gen [schema options]
  (let [mode (::-*-gen-mode options :*)
        options (dissoc options ::-*-gen-mode)]
    (->> options
         ;; TODO move to solver
         (-intersect-solutions [{:type :sequential
                                 :min-count (case mode
                                              :* 0
                                              :+ 1)}])
         (-solve-each (fn [solution options]
                        (let [child (-child schema options)]
                          (when-some [coerce (-coerce-regex-result solution)]
                            (cond->> (gen-vector (when (= :+ (::-*-gen-mode options)) {:min 1}) (generator child (dissoc options ::-*-gen-mode)))
                              (m/-regex-op? child)
                              (gen-fmap (comp coerce #(apply concat %)))

                              (and (not (m/-regex-op? child))
                                   (some-> (:type solution) (not= :vector)))
                              (gen-fmap coerce)))))))))

(defn -+-gen [schema options]
  (-*-gen schema (assoc options ::-*-gen-mode :+)))

(defn -repeat-gen [schema options]
  (-solve-each (fn [solution options]
                 (let [child (-child schema options)]
                   (when-some [coerce (-coerce-regex-result solution)]
                     (if-some [g (-not-unreachable
                                   (-coll-gen schema
                                              (-intersect-solutions [(if (m/-regex-op? child) {:type :sequential} solution)] options)))]
                       (cond->> g
                         (m/-regex-op? child)
                         (gen/fmap (comp coerce #(apply concat %))))
                       (gen/return (coerce ()))))))
               options))

(defn -qualified-ident-gen [schema mk-value-with-ns value-with-ns-gen-size pred gen]
  (if-let [namespace-unparsed (:namespace (m/properties schema))]
    (gen-fmap (fn [k] (mk-value-with-ns (name namespace-unparsed) (name k))) value-with-ns-gen-size)
    (gen-such-that schema pred gen)))

(defn -qualified-keyword-gen [schema]
  (-qualified-ident-gen schema keyword gen/keyword qualified-keyword? gen/keyword-ns))

(defn -qualified-symbol-gen [schema]
  (-qualified-ident-gen schema symbol gen/symbol qualified-symbol? gen/symbol-ns))

(defn- gen-elements [es]
  (if (= 1 (count es))
    (gen/return (first es))
    (gen/elements es)))

(defn- double-gen [schema options]
  (gen/double* (merge (let [props (m/properties schema options)]
                        {:infinite? (get props :gen/infinite? false)
                         :NaN? (get props :gen/NaN? false)})
                      (-> (-min-max schema options)
                          (update :min #(some-> % double))
                          (update :max #(some-> % double))))))

(defmulti -schema-generator (fn [schema options] (m/type schema options)) :default ::default)

(defmethod -schema-generator ::default [schema options] (ga/gen-for-pred (m/validator schema options)))

(defmethod -schema-generator 'pos-int? [_ options] (-int-gen* {:min 1} options))
(defmethod -schema-generator 'neg-int? [_ options] (-int-gen* {:max -1} options))
(defmethod -schema-generator 'nat-int? [_ options] (-int-gen* {:min 0} options))
(defmethod -schema-generator 'float? [_ options] (-double-gen* nil options))
(defmethod -schema-generator 'integer? [_ options] (-int-gen* nil options))
(defmethod -schema-generator 'int? [_ options] (-int-gen* nil options))
(defmethod -schema-generator 'number? [schema options] (-number-gen* nil options))
(defmethod -schema-generator :> [schema options] (-number-gen* {:min (inc (-child schema options))} options))
(defmethod -schema-generator :>= [schema options] (-number-gen* {:min (-child schema options)} options))
(defmethod -schema-generator :< [schema options] (-number-gen* {:max (dec (-child schema options))} options))
(defmethod -schema-generator :<= [schema options] (-number-gen* {:max (-child schema options)} options))
(defmethod -schema-generator := [schema options] (gen/return (-child schema options)))
(defmethod -schema-generator :not= [schema options] (gen-such-that schema #(not= % (-child schema options)) gen/any-printable))
(defmethod -schema-generator 'pos? [_ options] (gen-one-of [(-number-gen* {:min 0.00001} options) (-int-gen* {:min 1} options)]))
(defmethod -schema-generator 'neg? [_ options] (gen-one-of [(-number-gen* {:max -0.00001} options) (-int-gen* {:max -1} options)]))
(defmethod -schema-generator :not [schema options] (gen-such-that schema (m/validator schema options) (ga/gen-for-pred any?)))
(defmethod -schema-generator :and [schema options] (-and-gen schema options))
(defmethod -schema-generator :or [schema options] (-or-gen schema options))
(defmethod -schema-generator :orn [schema options] (-or-gen (m/into-schema :or (m/properties schema) (map last (m/children schema)) (m/options schema)) options))
(defmethod -schema-generator ::m/val [schema options] (-child-gen schema options))
(defmethod -schema-generator :map [schema options] (-map-gen schema options))
(defmethod -schema-generator :map-of [schema options] (-map-of-gen schema options))
(defmethod -schema-generator :multi [schema options] (-multi-gen schema options))
(defmethod -schema-generator :vector [schema options] (-coll-gen schema options))
(defmethod -schema-generator :sequential [schema options] (-coll-gen schema options))
(defmethod -schema-generator :set [schema options] (-coll-distinct-gen schema set options))
(defmethod -schema-generator :enum [schema options] (gen-elements (m/children schema options)))
(defmethod -schema-generator :seqable [schema options] (-seqable-gen schema options))
(defmethod -schema-generator :every [schema options] (-seqable-gen schema options)) ;;infinite seqs?
(defmethod -schema-generator :maybe [schema options] (gen-maybe (-child-gen schema options)))
(defmethod -schema-generator :tuple [schema options] (gen-tuple (map #(generator % options) (m/children schema options))))
#?(:clj (defmethod -schema-generator :re [schema options] (-re-gen schema options)))

(defn -any-gen* [options]
  (-solve-each (fn [solution options]
                 (let [options (assoc options ::solutions [solution])]
                   (case (:type solution)
                     nil (ga/gen-for-pred any?)
                     (:int :double :number) (generator number? options)
                     (:vector) (generator vector? options)
                     (do (println "Missing case for -any-gen*" (:type solution))
                         (ga/gen-for-pred any?)))))
               options))
(defmethod -schema-generator :any [_ options] (-any-gen* options))

(defmethod -schema-generator :some [_ _] gen/any-printable)
(defmethod -schema-generator :nil [_ _] nil-gen)
(defmethod -schema-generator :string [schema options] (-string-gen schema options))
(defmethod -schema-generator :int [schema options] (gen/large-integer* (-min-max schema options)))
(defmethod -schema-generator :double [schema options] (double-gen schema options))
(defmethod -schema-generator :float [schema options] (double-gen schema options))
(defmethod -schema-generator :boolean [_ _] gen/boolean)
(defmethod -schema-generator :keyword [_ _] gen/keyword)
(defmethod -schema-generator :symbol [_ _] gen/symbol)
(defmethod -schema-generator :qualified-keyword [schema _] (-qualified-keyword-gen schema))
(defmethod -schema-generator :qualified-symbol [schema _] (-qualified-symbol-gen schema))
(defmethod -schema-generator :uuid [_ _] gen/uuid)

(defmethod -schema-generator :=> [schema options] (-=>-gen schema options))
(defmethod -schema-generator :-> [schema options] (-=>-gen schema options))
(defmethod -schema-generator :function [schema options] (-function-gen schema options))
(defmethod -schema-generator 'ifn? [_ _] gen/keyword)
(defmethod -schema-generator :ref [schema options] (-ref-gen schema options))
(defmethod -schema-generator :schema [schema options] (generator (m/deref schema) options))
(defmethod -schema-generator ::m/schema [schema options] (generator (m/deref schema) options))

(defmethod -schema-generator :merge [schema options] (generator (m/deref schema) options))
(defmethod -schema-generator :union [schema options] (generator (m/deref schema) options))
(defmethod -schema-generator :select-keys [schema options] (generator (m/deref schema) options))

(defmethod -schema-generator :cat [schema options] (-cat-gen schema options))
(defmethod -schema-generator :catn [schema options] (-cat-gen schema options))
(defmethod -schema-generator :alt [schema options] (-alt-gen schema options))
(defmethod -schema-generator :altn [schema options] (-alt-gen schema options))

(defmethod -schema-generator :? [schema options] (-?-gen schema options))
(defmethod -schema-generator :* [schema options] (-*-gen schema options))
(defmethod -schema-generator :+ [schema options] (-+-gen schema options))
(defmethod -schema-generator :repeat [schema options] (-repeat-gen schema options))

;;
;; Creating a generator by different means, centralized under [[-create]]
;;

(defn- -create-from-return [props]
  (when (contains? props :gen/return)
    (gen/return (:gen/return props))))

(defn- -create-from-elements [props]
  (some-> (:gen/elements props) gen-elements))

(extend-protocol Generator
  #?(:clj Object, :cljs default)
  (-generator [schema options]
    (-schema-generator schema (-intersect-solutions (-solve-schema schema options) (assoc options ::original-generator-schema schema)))))

(defn- -create-from-gen
  [props schema options]
  (or (:gen/gen props)
      (when-not (:gen/elements props)
        (-generator schema options))))

(defn- -create-from-schema [props options]
  (some-> (:gen/schema props) (generator options)))

(defn- -create-from-fmap [gen props schema options]
  (when-some [fmap (:gen/fmap props)]
    (gen/fmap (m/eval fmap (or options (m/options schema)))
              gen)))

(defn- -create [schema options]
  (let [props (-merge (m/type-properties schema)
                      (m/properties schema))
        gen (or (-create-from-return props)
                (-create-from-elements props)
                (-create-from-schema props options)
                (-create-from-gen props schema options)
                (m/-fail! ::no-generator {:options options
                                          :schema schema}))]
    (or (-create-from-fmap gen props schema options)
        gen)))

;;
;; public api
;;

(defn generator
  ([?schema]
   (generator ?schema nil))
  ([?schema options]
   (if (::rec-gen options)
     ;; disable cache while calculating recursive schemas. caches don't distinguish options.
     (-create (m/schema ?schema options) options)
     (m/-cached (m/schema ?schema options) :generator #(-create % options)))))

(defn generate
  ([?gen-or-schema]
   (generate ?gen-or-schema nil))
  ([?gen-or-schema {:keys [seed size] :or {size 30} :as options}]
   (let [gen (if (gen/generator? ?gen-or-schema) ?gen-or-schema (generator ?gen-or-schema options))]
     (rose/root (gen/call-gen gen (-random seed) size)))))

(defn sample
  ([?gen-or-schema]
   (sample ?gen-or-schema nil))
  ([?gen-or-schema {:keys [seed size] :or {size 10} :as options}]
   (let [gen (if (gen/generator? ?gen-or-schema) ?gen-or-schema (generator ?gen-or-schema options))]
     (->> (gen/make-size-range-seq size)
          (map #(rose/root (gen/call-gen gen %1 %2))
               (gen/lazy-random-states (-random seed)))
          (take size)))))

;;
;; functions
;;

(defn function-checker
  ([?schema] (function-checker ?schema nil))
  ([?schema {::keys [=>iterations] :or {=>iterations 100} :as options}]
   (let [schema (m/schema ?schema options)
         -try (fn [f] (try [(f) true] (catch #?(:clj Exception, :cljs js/Error) e [e false])))
         check (fn [schema]
                 (let [{:keys [input output guard]} (m/-function-info schema)
                       input-generator (generator input options)
                       valid-output? (m/validator output options)
                       valid-guard? (if guard (m/validator guard options) (constantly true))
                       validate (fn [f args] (as-> (apply f args) $ (and (valid-output? $) (valid-guard? [args $]))))]
                   (fn [f]
                     (let [{:keys [result shrunk]} (->> (prop/for-all* [input-generator] #(validate f %))
                                                        (check/quick-check =>iterations))
                           smallest (-> shrunk :smallest first)]
                       (when-not (true? result)
                         (let [explain-input (m/explain input smallest)
                               [result success] (when-not explain-input (-try (fn [] (apply f smallest))))
                               explain-output (when (and success (not explain-input)) (m/explain output result))
                               explain-guard (when (and success guard (not explain-output)) (m/explain guard [smallest result]))]
                           (cond-> (assoc shrunk ::m/result result)
                             explain-input (assoc ::m/explain-input explain-input)
                             explain-output (assoc ::m/explain-output explain-output)
                             explain-guard (assoc ::m/explain-guard explain-guard)
                             (ex-message result) (-> (update :result ex-message) (dissoc :result-data)))))))))]
     (if (m/-function-info schema)
       (check schema)
       (if (m/-function-schema? schema)
         (let [checkers (map #(function-checker % options) (m/-function-schema-arities schema))]
           (fn [x] (->> checkers (keep #(% x)) (seq))))
         (m/-fail! ::invalid-function-schema {:type (m/-type schema)}))))))

(defn check
  ([?schema f] (check ?schema f nil))
  ([?schema f options]
   (let [schema (m/schema ?schema options)]
     (m/explain (m/-update-options schema #(assoc % ::m/function-checker function-checker)) f))))
