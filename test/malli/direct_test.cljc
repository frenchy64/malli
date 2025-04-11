(ns malli.direct-test
  (:require [malli.direct :as md]
            [malli.core :as m]
            [clojure.test :refer [is deftest]]))

(defn ->dgensym []
  (let [counter (atom -1)]
    (fn
      ([] (symbol (str "G__" (swap! counter inc))))
      ([n] (symbol (str (name n) (swap! counter inc)))))))

(defmacro with-deterministic-gensym
  [& body]
  `(let [gensym# (->dgensym)]
     (with-bindings {#'md/*gensym* gensym#}
       (do ~@body))))

(deftest direct-atomic-test
  (with-deterministic-gensym
    (is (= '(clojure.core/let
              [r0 {:registry (r/-registry)}]
              (malli.core/-into-schema
                (malli.registry/schema (:registry r0) ':int)
                'nil
                []
                r0))
           (md/direct :int))))
  (is (= :int (m/form (eval (md/direct :int))))))

(set! *print-namespace-maps* false)

(deftest direct-registry-test
  (with-deterministic-gensym
    (is (= '(clojure.core/let
              [r0 {:registry (malli.core/-registry)}]
              (clojure.core/let
                [r1
                 {':malli.direct-test/foo
                  (malli.core/-into-schema
                    (malli.registry/schema (:registry r0) ':int)
                    'nil
                    []
                    r0)}
                 r0
                 (malli.core/-update
                   r0
                   :registry
                   (clojure.core/fn
                     [x2]
                     (malli.registry/composite-registry
                       r1
                       (clojure.core/or x2 (malli.core/-registry r0)))))
                 p3
                 (clojure.core/assoc '{} :registry r1)]
                (malli.core/-into-schema
                  (malli.registry/schema (:registry r0) ':int)
                  p3
                  []
                  r0)))
           (md/direct [:int {:registry {::foo :int}}]))))
  (is (= [:int {:registry {:malli.direct-test/foo :int}}]
         (m/form (eval (md/direct [:int {:registry {::foo :int}}]))))))
