(ns malli.subtype
  "Subtyping algorithm for Malli schemas.
   
   A schema S is a subtype of T if (m/validator S) admits a subset of 
   values that (m/validator T) does."
  (:require [malli.core :as m]))

;;
;; Protocols
;;

;; Forward declarations
(declare subtype?* subtype-same?* subtype-left?* subtype-right?*)

;;
;; Core algorithm
;;

(defn subtype?
  "Returns true if schema S is a subtype of schema T.
   
   A schema S is a subtype of T if every value that validates against S
   also validates against T. In other words, (m/validator S) admits a 
   subset of values that (m/validator T) does.
   
   Options:
   - opts: schema options map (same as m/schema)"
  ([S T]
   (subtype? S T nil))
  ([S T opts]
   (let [S-schema (m/schema S opts)
         T-schema (m/schema T opts)]
     (subtype?* S-schema T-schema opts))))

(defn subtype?*
  "Core subtyping algorithm. Takes Schema instances.
   
   Dispatches to:
   - subtype-same?* when (= (m/-type S) (m/-type T))
   - subtype-left?* otherwise"
  [S T opts]
  (let [S-type (m/-type (m/-parent S))
        T-type (m/-type (m/-parent T))]
    (if (= S-type T-type)
      (subtype-same?* S-type S T opts)
      (subtype-left?* S-type S T opts))))

;;
;; Multimethods
;;

(defmulti subtype-same?*
  "Multimethod for when both schemas have the same type.
   Dispatches on the schema type.
   
   Arguments: [type S T opts]
   - type: The schema type (keyword)
   - S: The subtype schema candidate
   - T: The supertype schema candidate
   - opts: Options map"
  (fn [type S T opts] type))

(defmulti subtype-left?*
  "Multimethod for when schemas have different types.
   Dispatches on the left (subtype candidate) schema type.
   
   Arguments: [S-type S T opts]
   - S-type: The subtype schema type (keyword)
   - S: The subtype schema candidate
   - T: The supertype schema candidate
   - opts: Options map"
  (fn [S-type S T opts] S-type))

(defmulti subtype-right?*
  "Multimethod dispatching on the right (supertype candidate) schema type.
   Called from subtype-left?* implementations.
   
   Arguments: [S-type T-type S T opts]
   - S-type: The subtype schema type (keyword)
   - T-type: The supertype schema type (keyword)
   - S: The subtype schema candidate
   - T: The supertype schema candidate
   - opts: Options map"
  (fn [S-type T-type S T opts] [S-type T-type]))

;;
;; Default implementations
;;

(defmethod subtype-same?* :default [type S T opts]
  ;; By default, for unknown types, conservatively return false
  false)

(defmethod subtype-left?* :default [S-type S T opts]
  ;; Dispatch to right multimethod
  (let [T-type (m/-type (m/-parent T))]
    (subtype-right?* S-type T-type S T opts)))

(defmethod subtype-right?* :default [S-type T-type S T opts]
  ;; By default, conservatively return false
  false)

;;
;; Helper functions
;;

(defn- properties-subtype?
  "Check if properties of S are compatible with properties of T.
   For numeric types with min/max constraints.
   S <: T means S is MORE restrictive than T."
  [S T]
  (let [S-props (m/-properties S)
        T-props (m/-properties T)
        S-min (get S-props :min)
        S-max (get S-props :max)
        T-min (get T-props :min)
        T-max (get T-props :max)]
    ;; S <: T when S's constraints are tighter than T's
    ;; For min: S-min >= T-min (S has a higher minimum)
    ;; For max: S-max <= T-max (S has a lower maximum)
    ;; If T has a constraint but S doesn't, S is not more restrictive
    (and (or (nil? T-min) (and S-min (>= S-min T-min)))
         (or (nil? T-max) (and S-max (<= S-max T-max))))))

;;
;; Same-type implementations
;;

;; :any is the top type - any subtype of :any must also be :any
(defmethod subtype-same?* :any [_ S T opts]
  true)

;; :nil - all nils are the same
(defmethod subtype-same?* :nil [_ S T opts]
  true)

;; :some - all somes are the same (any non-nil value)
(defmethod subtype-same?* :some [_ S T opts]
  true)

;; Simple types with potential min/max properties
(defmethod subtype-same?* :string [_ S T opts]
  (properties-subtype? S T))

(defmethod subtype-same?* :int [_ S T opts]
  (properties-subtype? S T))

(defmethod subtype-same?* :float [_ S T opts]
  (properties-subtype? S T))

(defmethod subtype-same?* :double [_ S T opts]
  (properties-subtype? S T))

(defmethod subtype-same?* :boolean [_ S T opts]
  true)

(defmethod subtype-same?* :keyword [_ S T opts]
  true)

(defmethod subtype-same?* :symbol [_ S T opts]
  true)

(defmethod subtype-same?* :qualified-keyword [_ S T opts]
  (let [S-props (m/-properties S)
        T-props (m/-properties T)
        S-ns (get S-props :namespace)
        T-ns (get T-props :namespace)]
    (or (nil? T-ns) (= S-ns T-ns))))

(defmethod subtype-same?* :qualified-symbol [_ S T opts]
  true)

(defmethod subtype-same?* :uuid [_ S T opts]
  true)

;; :and - S is subtype of T if all of T's children are supertypes of S
(defmethod subtype-same?* :and [_ S T opts]
  (let [S-children (m/-children S)
        T-children (m/-children T)]
    ;; S <: T if T's constraints are a subset of S's constraints
    ;; For every child in T, S (as a whole) must satisfy it
    ;; This means: for all T-child, S <: T-child
    ;; Which means: [:and ...S-children...] <: T-child
    ;; Conservatively: check if some S-child <: T-child or all S-children together <: T-child
    (every? (fn [T-child]
              ;; S = [:and S1 S2 ...] <: T-child if any Si <: T-child
              ;; or if the intersection of all Si is <: T-child
              (some (fn [S-child]
                      (subtype?* S-child T-child opts))
                    S-children))
            T-children)))

;; :or - S is subtype of T if S's union is a subset of T's union
(defmethod subtype-same?* :or [_ S T opts]
  (let [S-children (m/-children S)
        T-children (m/-children T)]
    ;; Every alternative in S must be a subtype of some alternative in T
    (every? (fn [S-child]
              (some (fn [T-child]
                      (subtype?* S-child T-child opts))
                    T-children))
            S-children)))

;; :orn - similar to :or but with tagged branches
(defmethod subtype-same?* :orn [_ S T opts]
  (let [S-entries (m/-children S)
        T-entries (m/-children T)]
    ;; For each entry in S, there must be a corresponding entry in T
    ;; where the child schemas are in subtype relation
    (every? (fn [[S-tag _ S-child]]
              (some (fn [[T-tag _ T-child]]
                      (and (= S-tag T-tag)
                           (subtype?* S-child T-child opts)))
                    T-entries))
            S-entries)))

;; :enum - S is subtype of T if S's values are a subset of T's values
(defmethod subtype-same?* :enum [_ S T opts]
  (let [S-children (set (m/-children S))
        T-children (set (m/-children T))]
    (every? T-children S-children)))

;; :maybe - S is subtype of T if S's child is subtype of T's child
(defmethod subtype-same?* :maybe [_ S T opts]
  (let [[S-child] (m/-children S)
        [T-child] (m/-children T)]
    (subtype?* S-child T-child opts)))

;; Collection types
(defmethod subtype-same?* :vector [_ S T opts]
  (let [[S-child] (m/-children S)
        [T-child] (m/-children T)
        S-props (m/-properties S)
        T-props (m/-properties T)
        S-min (get S-props :min)
        S-max (get S-props :max)
        T-min (get T-props :min)
        T-max (get T-props :max)]
    (and S-child T-child
         (subtype?* S-child T-child opts)
         ;; S is more restrictive on size
         (or (nil? T-min) (and S-min (>= S-min T-min)))
         (or (nil? T-max) (and S-max (<= S-max T-max))))))

(defmethod subtype-same?* :sequential [_ S T opts]
  (let [[S-child] (m/-children S)
        [T-child] (m/-children T)
        S-props (m/-properties S)
        T-props (m/-properties T)
        S-min (get S-props :min)
        S-max (get S-props :max)
        T-min (get T-props :min)
        T-max (get T-props :max)]
    (and S-child T-child
         (subtype?* S-child T-child opts)
         (or (nil? T-min) (and S-min (>= S-min T-min)))
         (or (nil? T-max) (and S-max (<= S-max T-max))))))

(defmethod subtype-same?* :set [_ S T opts]
  (let [[S-child] (m/-children S)
        [T-child] (m/-children T)
        S-props (m/-properties S)
        T-props (m/-properties T)
        S-min (get S-props :min)
        S-max (get S-props :max)
        T-min (get T-props :min)
        T-max (get T-props :max)]
    (and S-child T-child
         (subtype?* S-child T-child opts)
         (or (nil? T-min) (and S-min (>= S-min T-min)))
         (or (nil? T-max) (and S-max (<= S-max T-max))))))

;; :tuple - S is subtype of T if all corresponding elements are subtypes
(defmethod subtype-same?* :tuple [_ S T opts]
  (let [S-children (m/-children S)
        T-children (m/-children T)]
    (and (= (count S-children) (count T-children))
         (every? identity
                 (map (fn [S-child T-child]
                        (subtype?* S-child T-child opts))
                      S-children T-children)))))

;; :map - S is subtype of T if S has all required keys of T with compatible types
(defmethod subtype-same?* :map [_ S T opts]
  (when (and (m/-entry-schema? S) (m/-entry-schema? T))
    (let [S-entries (into {} (map (fn [e]
                                    (let [k (key e)
                                          v (val e)]
                                      [k {:props (m/-properties v)
                                          :schema (first (m/-children v))}])))
                          (m/-entries S))
          T-entries (into {} (map (fn [e]
                                    (let [k (key e)
                                          v (val e)]
                                      [k {:props (m/-properties v)
                                          :schema (first (m/-children v))}])))
                          (m/-entries T))]
      ;; For each required entry in T, S must have a compatible entry
      (every? (fn [[k {:keys [props schema]}]]
                (if (get props :optional)
                  true  ;; Optional entries in T don't constrain S
                  (when-let [S-entry (get S-entries k)]
                    (subtype?* (:schema S-entry) schema opts))))
              T-entries))))

;; :map-of - S is subtype of T if S's key/value types are subtypes of T's
(defmethod subtype-same?* :map-of [_ S T opts]
  (let [[S-key-schema S-val-schema] (m/-children S)
        [T-key-schema T-val-schema] (m/-children T)
        S-props (m/-properties S)
        T-props (m/-properties T)
        S-min (get S-props :min)
        S-max (get S-props :max)
        T-min (get T-props :min)
        T-max (get T-props :max)]
    (and (subtype?* S-key-schema T-key-schema opts)
         (subtype?* S-val-schema T-val-schema opts)
         (or (nil? T-min) (and S-min (>= S-min T-min)))
         (or (nil? T-max) (and S-max (<= S-max T-max))))))

;; :not - S is subtype of T if S's child is a supertype of T's child (contravariance)
(defmethod subtype-same?* :not [_ S T opts]
  ;; :not is contravariant - [:not S-child] <: [:not T-child] when T-child <: S-child
  ;; S accepts "not S-child", T accepts "not T-child"
  ;; For S <: T, we need (not S-child) ⊆ (not T-child)
  ;; This is equivalent to T-child ⊆ S-child
  (let [[S-child] (m/-children S)
        [T-child] (m/-children T)]
    (subtype?* T-child S-child opts)))

;; :ref - dereference and check
(defmethod subtype-same?* :ref [_ S T opts]
  (subtype?* (m/-deref S) (m/-deref T) opts))

;; Predicate schemas
(defmethod subtype-same?* :fn [_ S T opts]
  ;; Two function schemas are equivalent if they're the same
  ;; We can't generally prove one function is a subset of another
  (= (m/-form S) (m/-form T)))

;; Regex schemas
(defmethod subtype-same?* :cat [_ S T opts]
  (let [S-children (m/-children S)
        T-children (m/-children T)]
    (and (= (count S-children) (count T-children))
         (every? identity
                 (map (fn [S-child T-child]
                        (subtype?* S-child T-child opts))
                      S-children T-children)))))

(defmethod subtype-same?* :alt [_ S T opts]
  (let [S-children (m/-children S)
        T-children (m/-children T)]
    ;; Every alternative in S must be a subtype of some alternative in T
    (every? (fn [S-child]
              (some (fn [T-child]
                      (subtype?* S-child T-child opts))
                    T-children))
            S-children)))

(defmethod subtype-same?* :* [_ S T opts]
  (let [[S-child] (m/-children S)
        [T-child] (m/-children T)]
    (subtype?* S-child T-child opts)))

(defmethod subtype-same?* :+ [_ S T opts]
  (let [[S-child] (m/-children S)
        [T-child] (m/-children T)]
    (subtype?* S-child T-child opts)))

(defmethod subtype-same?* :? [_ S T opts]
  (let [[S-child] (m/-children S)
        [T-child] (m/-children T)]
    (subtype?* S-child T-child opts)))

(defmethod subtype-same?* :repeat [_ S T opts]
  (let [[S-child] (m/-children S)
        [T-child] (m/-children T)
        S-props (m/-properties S)
        T-props (m/-properties T)]
    (and (subtype?* S-child T-child opts)
         (properties-subtype? S T))))

;;
;; Left-dispatch implementations (different types)
;;

;; :any on the right is the top type - everything is a subtype of :any
(defmethod subtype-right?* [:any :any] [_ _ S T opts] true)
(defmethod subtype-right?* [:some :any] [_ _ S T opts] true)
(defmethod subtype-right?* [:nil :any] [_ _ S T opts] true)
(defmethod subtype-right?* [:string :any] [_ _ S T opts] true)
(defmethod subtype-right?* [:int :any] [_ _ S T opts] true)
(defmethod subtype-right?* [:float :any] [_ _ S T opts] true)
(defmethod subtype-right?* [:double :any] [_ _ S T opts] true)
(defmethod subtype-right?* [:boolean :any] [_ _ S T opts] true)
(defmethod subtype-right?* [:keyword :any] [_ _ S T opts] true)
(defmethod subtype-right?* [:symbol :any] [_ _ S T opts] true)
(defmethod subtype-right?* [:qualified-keyword :any] [_ _ S T opts] true)
(defmethod subtype-right?* [:qualified-symbol :any] [_ _ S T opts] true)
(defmethod subtype-right?* [:uuid :any] [_ _ S T opts] true)
(defmethod subtype-right?* [:vector :any] [_ _ S T opts] true)
(defmethod subtype-right?* [:sequential :any] [_ _ S T opts] true)
(defmethod subtype-right?* [:set :any] [_ _ S T opts] true)
(defmethod subtype-right?* [:tuple :any] [_ _ S T opts] true)
(defmethod subtype-right?* [:map :any] [_ _ S T opts] true)
(defmethod subtype-right?* [:map-of :any] [_ _ S T opts] true)
(defmethod subtype-right?* [:and :any] [_ _ S T opts] true)
(defmethod subtype-right?* [:or :any] [_ _ S T opts] true)
(defmethod subtype-right?* [:orn :any] [_ _ S T opts] true)
(defmethod subtype-right?* [:enum :any] [_ _ S T opts] true)
(defmethod subtype-right?* [:maybe :any] [_ _ S T opts] true)
(defmethod subtype-right?* [:not :any] [_ _ S T opts] true)
(defmethod subtype-right?* [:fn :any] [_ _ S T opts] true)
(defmethod subtype-right?* [:cat :any] [_ _ S T opts] true)
(defmethod subtype-right?* [:alt :any] [_ _ S T opts] true)
(defmethod subtype-right?* [:* :any] [_ _ S T opts] true)
(defmethod subtype-right?* [:+ :any] [_ _ S T opts] true)
(defmethod subtype-right?* [:? :any] [_ _ S T opts] true)
(defmethod subtype-right?* [:repeat :any] [_ _ S T opts] true)

;; :some on the right - everything except :nil is a subtype of :some
(defmethod subtype-right?* [:some :some] [_ _ S T opts] true)
(defmethod subtype-right?* [:string :some] [_ _ S T opts] true)
(defmethod subtype-right?* [:int :some] [_ _ S T opts] true)
(defmethod subtype-right?* [:float :some] [_ _ S T opts] true)
(defmethod subtype-right?* [:double :some] [_ _ S T opts] true)
(defmethod subtype-right?* [:boolean :some] [_ _ S T opts] true)
(defmethod subtype-right?* [:keyword :some] [_ _ S T opts] true)
(defmethod subtype-right?* [:symbol :some] [_ _ S T opts] true)
(defmethod subtype-right?* [:qualified-keyword :some] [_ _ S T opts] true)
(defmethod subtype-right?* [:qualified-symbol :some] [_ _ S T opts] true)
(defmethod subtype-right?* [:uuid :some] [_ _ S T opts] true)
(defmethod subtype-right?* [:vector :some] [_ _ S T opts] true)
(defmethod subtype-right?* [:sequential :some] [_ _ S T opts] true)
(defmethod subtype-right?* [:set :some] [_ _ S T opts] true)
(defmethod subtype-right?* [:tuple :some] [_ _ S T opts] true)
(defmethod subtype-right?* [:map :some] [_ _ S T opts] true)
(defmethod subtype-right?* [:map-of :some] [_ _ S T opts] true)
(defmethod subtype-right?* [:enum :some] [_ _ S T opts] 
  ;; enum <: some if none of the values are nil
  (not-any? nil? (m/-children S)))
(defmethod subtype-right?* [:and :some] [_ _ S T opts]
  ;; :and <: :some if all children are <: :some
  (every? #(subtype?* % T opts) (m/-children S)))
(defmethod subtype-right?* [:or :some] [_ _ S T opts]
  ;; :or <: :some if all alternatives are <: :some
  (every? #(subtype?* % T opts) (m/-children S)))

;; Numeric hierarchy: int <: double, int <: float, etc.
(defmethod subtype-right?* [:int :double] [_ _ S T opts]
  (properties-subtype? S T))
(defmethod subtype-right?* [:int :float] [_ _ S T opts]
  (properties-subtype? S T))
(defmethod subtype-right?* [:float :double] [_ _ S T opts]
  (properties-subtype? S T))

;; Collection hierarchy
(defmethod subtype-right?* [:vector :sequential] [_ _ S T opts]
  (let [[S-child] (m/-children S)
        [T-child] (m/-children T)]
    (if (and S-child T-child)
      (and (subtype?* S-child T-child opts)
           (properties-subtype? S T))
      true)))

(defmethod subtype-right?* [:tuple :sequential] [_ _ S T opts]
  (let [S-children (m/-children S)
        [T-child] (m/-children T)]
    (if T-child
      (every? #(subtype?* % T-child opts) S-children)
      true)))

(defmethod subtype-right?* [:tuple :vector] [_ _ S T opts]
  (let [S-children (m/-children S)
        [T-child] (m/-children T)]
    (if T-child
      (every? #(subtype?* % T-child opts) S-children)
      true)))

;; :maybe relationships
(defmethod subtype-right?* [:nil :maybe] [_ _ S T opts]
  true)

(defmethod subtype-right?* [:some :maybe] [_ _ S T opts]
  (let [[T-child] (m/-children T)]
    (subtype?* S T-child opts)))

(defmethod subtype-right?* [:and :maybe] [_ _ S T opts]
  (let [[T-child] (m/-children T)]
    ;; Check if any child of S ensures it's a subtype of [:maybe T-child]
    ;; This is true if S is guaranteed to produce values that are either nil or T-child
    (some (fn [S-child]
            (or (subtype?* S-child (m/schema :nil) opts)
                (subtype?* S-child T-child opts)))
          (m/-children S))))

;; :and on the right - S <: [:and T1 T2 ...] if S <: T1 and S <: T2 and ...
(defmethod subtype-right?* [:nil :and] [_ _ S T opts]
  (every? #(subtype?* S % opts) (m/-children T)))
(defmethod subtype-right?* [:some :and] [_ _ S T opts]
  (every? #(subtype?* S % opts) (m/-children T)))
(defmethod subtype-right?* [:string :and] [_ _ S T opts]
  (every? #(subtype?* S % opts) (m/-children T)))
(defmethod subtype-right?* [:int :and] [_ _ S T opts]
  (every? #(subtype?* S % opts) (m/-children T)))
(defmethod subtype-right?* [:double :and] [_ _ S T opts]
  (every? #(subtype?* S % opts) (m/-children T)))
(defmethod subtype-right?* [:float :and] [_ _ S T opts]
  (every? #(subtype?* S % opts) (m/-children T)))
(defmethod subtype-right?* [:keyword :and] [_ _ S T opts]
  (every? #(subtype?* S % opts) (m/-children T)))
(defmethod subtype-right?* [:symbol :and] [_ _ S T opts]
  (every? #(subtype?* S % opts) (m/-children T)))
(defmethod subtype-right?* [:vector :and] [_ _ S T opts]
  (every? #(subtype?* S % opts) (m/-children T)))
(defmethod subtype-right?* [:map :and] [_ _ S T opts]
  (every? #(subtype?* S % opts) (m/-children T)))
(defmethod subtype-right?* [:and :and] [_ _ S T opts]
  ;; [:and S1 S2] <: [:and T1 T2] if for all Ti, there exists Sj where Sj <: Ti
  (every? (fn [T-child]
            (some #(subtype?* % T-child opts) (m/-children S)))
          (m/-children T)))

;; [:and ...] on the left (subtype) - check if it's a subtype of the right
(defmethod subtype-left?* :and [_ S T opts]
  (let [T-type (m/-type (m/-parent T))]
    ;; [:and S1 S2 ...] <: T if ALL Si <: T
    ;; This is conservative but sound
    (if (= T-type :and)
      ;; Special case: :and <: :and handled by same-type
      (subtype-same?* :and S T opts)
      ;; General case: all children must be subtypes of T
      (every? #(subtype?* % T opts) (m/-children S)))))

;; :or on the right - S <: [:or T1 T2 ...] if S <: T1 or S <: T2 or ...
(defmethod subtype-right?* [:nil :or] [_ _ S T opts]
  (some #(subtype?* S % opts) (m/-children T)))
(defmethod subtype-right?* [:some :or] [_ _ S T opts]
  (some #(subtype?* S % opts) (m/-children T)))
(defmethod subtype-right?* [:string :or] [_ _ S T opts]
  (some #(subtype?* S % opts) (m/-children T)))
(defmethod subtype-right?* [:int :or] [_ _ S T opts]
  (some #(subtype?* S % opts) (m/-children T)))
(defmethod subtype-right?* [:double :or] [_ _ S T opts]
  (some #(subtype?* S % opts) (m/-children T)))
(defmethod subtype-right?* [:float :or] [_ _ S T opts]
  (some #(subtype?* S % opts) (m/-children T)))
(defmethod subtype-right?* [:boolean :or] [_ _ S T opts]
  (some #(subtype?* S % opts) (m/-children T)))
(defmethod subtype-right?* [:keyword :or] [_ _ S T opts]
  (some #(subtype?* S % opts) (m/-children T)))
(defmethod subtype-right?* [:symbol :or] [_ _ S T opts]
  (some #(subtype?* S % opts) (m/-children T)))
(defmethod subtype-right?* [:vector :or] [_ _ S T opts]
  (some #(subtype?* S % opts) (m/-children T)))
(defmethod subtype-right?* [:map :or] [_ _ S T opts]
  (some #(subtype?* S % opts) (m/-children T)))
(defmethod subtype-right?* [:and :or] [_ _ S T opts]
  (some #(subtype?* S % opts) (m/-children T)))
(defmethod subtype-right?* [:or :or] [_ _ S T opts]
  ;; [:or S1 S2] <: [:or T1 T2] if for all Si, there exists Tj where Si <: Tj
  (every? (fn [S-child]
            (some #(subtype?* S-child % opts) (m/-children T)))
          (m/-children S)))

;; Make sure we return false (not nil) for unhandled cases
(defmethod subtype-right?* :default [S-type T-type S T opts]
  false)

;; :enum relationships
(defmethod subtype-right?* [:string :enum] [_ _ S T opts]
  false)  ;; A general string is not a subtype of a specific enum
(defmethod subtype-right?* [:int :enum] [_ _ S T opts]
  false)
(defmethod subtype-right?* [:keyword :enum] [_ _ S T opts]
  false)

;; Specific enum cases
(defmethod subtype-right?* [:enum :or] [_ _ S T opts]
  ;; An enum is a subtype of :or if all enum values are subtypes of some :or branch
  (let [enum-children (m/-children S)
        or-children (m/-children T)]
    (every? (fn [enum-val]
              ;; Check if this enum value would validate against any branch of the :or
              (some (fn [or-child]
                      (m/validate or-child enum-val))
                    or-children))
            enum-children)))

;; :keyword and :qualified-keyword relationship
(defmethod subtype-right?* [:qualified-keyword :keyword] [_ _ S T opts]
  true)
