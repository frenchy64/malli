(ns ^:no-doc malli.dev.cljs-direct-preload
  "Shadow-cljs preload for browser builds, used to save AOT compilation results for CLJS schemas to disk during development."
  #?(:cljs (:require-macros [malli.dev.cljs-kondo-preload]))
  (:require [malli.direct :as md]
            #?@(:cljs
                [[shadow.cljs.devtools.client.shared :as client.shared]
                 [shadow.cljs.devtools.client.env :as env]
                 [shadow.remote.runtime.api :as api]
                 [shadow.remote.runtime.shared :as runtime]])
            #?@(:clj
                [[shadow.cljs.devtools.server.worker.impl :as worker]])))

#?(:cljs
   (defn send-compiled-schemas-to-shadow!
     "During development sends AOT compiled schemas for all collected functions with malli schemas to the shadow-cljs clojure runtime which writes it to disk."
     {:dev/after-load true}
     []
     (runtime/relay-msg
      @client.shared/runtime-ref
      {:op ::md/write-compiled-schemas
       :to env/worker-client-id
       :build-id (keyword env/build-id)
       :data (md/get-compiled-schemas)})))

;; The following sends the config on first load of the app, the above function handles hot-reloads.

#?(:cljs
   (client.shared/add-plugin!
    ::client #{}
    (fn [{:keys [runtime] :as env}]
      (api/add-extension runtime ::client
                         {:on-welcome
                          (fn [] (send-compiled-schemas-to-shadow!))

                          :on-disconnect
                          (fn [e])

                          :on-reconnect
                          (fn [e] (send-compiled-schemas-to-shadow!))})
      env)

    (fn [{:keys [runtime]}]
      (api/del-extension runtime ::client))))

#?(:clj
   (defmethod worker/do-relay-msg ::md/write-compiled-schemas
     [worker-state msg]
     (clj-kondo/save! (:data msg) :cljs)
     worker-state))
