(ns donut.endpoint.test.harness
  "Includes:

  * Macros and a fixture for dealing with a system for the duration of a test
  * Helpers for composing and dispatching requests
  * `read-body` multimethod for parsing response bodies of different types (transit, json etc)
  * assertions that work with response segments"
  (:require [donut.system :as ds]
            [meta-merge.core :as mm]
            [muuntaja.core :as m]
            [reitit.core :as rc]
            [ring.mock.request :as mock]))

(def ^:dynamic *system* nil)

(def ConfigurationComponentGroup
  {:component-ids                (ds/const {:router  [:http :router]
                                            :handler [:http :handler]})
   :default-request-content-type :transit-json})

;; -------------------------
;; system wrapper helpers
;; -------------------------

(defn system
  "more assertive system retrieval"
  []
  (when-not *system*
    (throw (ex-info "donut.endpoint.test.harness/*system* is nil but should be a system. Try adding (use-fixtures :each (system-fixture :test-system-name)) to your test namespace." {})))
  *system*)

(defmacro with-system
  "Bind dynamic system var to a test system."
  [[config-name custom-config] & body]
  `(let [conf# (-> (ds/config ~config-name)
                   (update-in [::ds/defs ::config]
                              #(mm/meta-merge ConfigurationComponentGroup %)))]
     (binding [*system* (ds/start conf# ~custom-config)]
       (let [return# (do ~@body)]
         (ds/stop *system*)
         return#))))

(defn system-fixture
  "To be used with `use-fixtures`"
  [config-name & [custom-config]]
  (fn [f]
    (with-system [config-name custom-config] (f))))

(defn instance
  "Look up component instance in current test system"
  [component-id & [err-msg]]
  (or (get-in (system) (into [::ds/instances] component-id))
      (throw (ex-info (or err-msg (str "Could not find component " component-id))
                      {:component-id component-id}))))

(defn configured-instance
  "Look up a component instance that has path configured by the `::config`
  component group"
  [human-name component-config-id]
  (let [component-id (instance [::config :component-ids component-config-id]
                               (str "There is no configured component id for "
                                    component-config-id ". See TODO on configuring the donut test harness."))]
    (instance component-id
              (format (str "No %s for *system* at %s. "
                           "See TODO on configuring the donut test harness to find your %s.")
                      human-name
                      component-id
                      human-name))))

(defn configured-value
  "Look up a component instance that has path configured by the `::config`
  component group"
  [config-key]
  (instance [::config config-key]
            (str "There is no config value for " config-key)))

;; ---
;; paths
;; ---

(defn router
  "The endpoint router."
  []
  (configured-instance "router" :router))

(defn path
  ([route-name]
   (path route-name {}))
  ([route-name route-params]
   ;; TODO assert that router is a router
   (or (:path (rc/match-by-name (router) route-name route-params))
       (throw (ex-info "could not generate router path" {:route-name   route-name
                                                         :route-params route-params})))))

;; -------------------------
;; compose and dispatch requests
;; -------------------------

(defn handler
  "The endpoint handler."
  []
  (configured-instance "handler" :handler))

(defn headers
  "Add all headers to request"
  [req headers]
  (reduce-kv mock/header req headers))


(defmulti base-request*
  (fn [_method _url _params content-type]
    content-type))

(defmethod base-request* :transit-json
  [method url params _]
  (-> (mock/request method url)
      (headers {:content-type "application/transit+json"
                :accept       "application/transit+json"})
      (assoc :body (m/encode "application/transit+json" params))))

(defmethod base-request* :json
  [method url params _]
  (-> (mock/request method url)
      (headers {:content-type "application/json"
                :accept       "application/json"})
      (assoc :body (m/encode "application/json" params))))

(defmethod base-request* :html
  [method url params _]
  (mock/request method url params))

(defn urlize
  [path-or-route-name & [params]]
  (if (keyword? path-or-route-name)
    (path path-or-route-name params)
    path-or-route-name))

(defn base-request**
  [method path-or-route-name params content-type]
  (base-request* method (urlize path-or-route-name params) params content-type))

(defn base-request
  ([method path-or-route-name]
   (base-request** method
                   path-or-route-name
                   {}
                   (configured-value :default-request-content-type)))
  ([method path-or-route-name params-or-content-type]
   (if (keyword? params-or-content-type)
     (base-request** method
                     path-or-route-name
                     {}
                     params-or-content-type)
     (base-request** method
                     path-or-route-name
                     params-or-content-type
                     (configured-value :default-request-content-type))))
  ([method path-or-route-name params content-type]
   (base-request** method path-or-route-name params content-type)))

(defn req
  "Perform a request with the system's root handler"
  [method path-or-route-name & args]
  ((handler) (apply base-request method path-or-route-name args)))

;; -------------------------
;; read responses
;; -------------------------

(defmulti read-body "Read body according to content type"
  (fn [{:keys [headers]}]
    (->> (or (get headers "Content-Type")
             (get headers "content-type"))
         (re-matches #"(.*?)(;.*)?")
         second)))

(defmethod read-body "application/transit+json"
  [{:keys [body]}]
  (m/decode "application/transit+json" body))

(defmethod read-body "application/json"
  [{:keys [body]}]
  (m/decode "application/json" body))

(defmethod read-body :default
  [{:keys [body]}]
  (if (string? body)
    body
    (slurp body)))
