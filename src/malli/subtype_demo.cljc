(ns malli.subtype-demo
  "Demonstration of the Malli subtyping algorithm."
  (:require [malli.core :as m]
            [malli.subtype :as ms]))

(comment
  ;; Basic type relationships
  (ms/subtype? :int :int)              ;=> true (reflexivity)
  (ms/subtype? :int :any)              ;=> true (:any is the top type)
  (ms/subtype? :int :some)             ;=> true (:some accepts non-nil values)
  (ms/subtype? :nil :some)             ;=> false (:nil is not non-nil)
  
  ;; Properties and constraints
  (ms/subtype? [:int {:min 5 :max 10}]
               [:int {:min 0 :max 20}]) ;=> true (more restrictive)
  (ms/subtype? [:int {:min 0 :max 20}]
               [:int {:min 5 :max 10}]) ;=> false (less restrictive)
  
  ;; Union types (:or)
  (ms/subtype? :int [:or :int :string])  ;=> true
  (ms/subtype? [:or :int :keyword]
               [:or :int :string :keyword]) ;=> true (subset)
  
  ;; Intersection types (:and)
  (ms/subtype? [:and :int [:> 0]]
               :int)                     ;=> true (more specific)
  (ms/subtype? [:and :int [:> 0]]
               [:or :int :string])       ;=> true
  
  ;; Enum types
  (ms/subtype? [:enum 1 2]
               [:enum 1 2 3])            ;=> true (subset of values)
  (ms/subtype? [:enum 1 2]
               [:or :int :string])       ;=> true (values match)
  
  ;; Collections
  (ms/subtype? [:vector :int]
               [:vector :int])           ;=> true
  (ms/subtype? [:vector [:int {:min 5}]]
               [:vector :int])           ;=> true (element type is subtype)
  (ms/subtype? [:tuple :int :string]
               [:vector :any])           ;=> true (tuples are vectors)
  (ms/subtype? [:tuple :int :string]
               [:sequential :any])       ;=> true
  
  ;; Maps
  (ms/subtype? [:map [:x :int] [:y :string]]
               [:map [:x :int]])         ;=> true (extra keys OK)
  (ms/subtype? [:map [:x :int]]
               [:map [:x :int] [:y :string]]) ;=> false (missing required key)
  (ms/subtype? [:map [:x :int]]
               [:map [:x :int] [:y {:optional true} :string]]) ;=> true (optional OK)
  
  ;; Negation (:not) - contravariant
  (ms/subtype? [:not :string]
               [:not :string])           ;=> true
  (ms/subtype? [:not [:int {:min 5}]]
               [:not :int])              ;=> false (contravariance)
  
  ;; Maybe (nullable)
  (ms/subtype? :nil [:maybe :int])      ;=> true
  (ms/subtype? [:maybe :int] :any)      ;=> true
  
  ;; Practical example: API schemas
  (def User [:map
             [:id :int]
             [:name :string]])
  
  (def AdminUser [:map
                  [:id :int]
                  [:name :string]
                  [:role :keyword]])
  
  ;; AdminUser has all required fields of User plus more
  (ms/subtype? AdminUser User)          ;=> true
  (ms/subtype? User AdminUser)          ;=> false
  
  ;; This means AdminUser values can be used where User values are expected
  )
