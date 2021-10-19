(ns donut.endpoint.test.harness
  "Includes:

  * Macros and a fixture for dealing with a system for the duration of a test
  * Helpers for composing and dispatching requests
  * `read-body` multimethod for parsing response bodies of different types (transit, json etc)
  * assertions that work with response segments"
  (:require [donut.system :as ds]
            [jsonista.core :as j]
            [muuntaja.core :as m]
            [reitit.core :as rc]
            [ring.mock.request :as mock]))

(def ^:dynamic *system* nil)

(def component-layout
  {:router  [:http :router]
   :handler [:http :handler]})

(def muuntaja (-> m/default-options
                  (assoc-in ["formats" "application/json" :decoder-opts] {:keywords? true})
                  m/create))

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
  `(binding [*system* (ds/start ~config-name ~custom-config)]
     (let [return# (do ~@body)]
       (ds/stop *system*)
       return#)))

(defn system-fixture
  "To be used with `use-fixtures`"
  [config-name custom-config]
  (fn [f]
    (with-system [config-name custom-config] (f))))

(defn component
  "Look up component in current test system"
  [component-id]
  (or (get-in (system) (into [::ds/instances] component-id))
      (throw (ex-info "Could not find component" {:component-id component-id}))))

;; ---
;; paths
;; ---

(defn router
  "The endpoint router."
  []
  (let [{:keys [router]} component-layout]
    (or (get-in (system) router)
        (throw (ex-info (str "No router for *system* at "
                             router
                             ". Add a router or call alter-var-root on component-layout to specify router component-id")
                        {})))))

(defn path
  ([route-name]
   (path route-name {}))
  ([route-name route-params]
   (or (:path (rc/match-by-name (router) route-name route-params))
       (throw (ex-info "could not generate router path" {:route-name   route-name
                                                         :route-params route-params})))))

;; -------------------------
;; compose and dispatch requests
;; -------------------------

(defn handler
  []
  (let [{:keys [handler]} component-layout]
    (or (get-in (system) handler)
        (throw (ex-info (str "No handler for *system* at "
                             router
                             ". Add a handler or call alter-var-root on component-layout to specify handler component-id")
                        {})))))

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
      (assoc :body (m/encode muuntaja "application/transit+json" params))))

(defmethod base-request* :json
  [method url params _]
  (-> (mock/request method url)
      (headers {:content-type "application/json"
                :accept       "application/json"})
      (assoc :body (m/encode muuntaja "application/json" params))))

(defmethod base-request* :html
  [method url params _]
  (mock/request method url params))

(defn urlize
  [url & [params]]
  (if (keyword? url) (path url params) url))

(defn base-request**
  [method url params content-type]
  (base-request* method (urlize url params) params content-type))

(defn base-request
  ([method url]
   (base-request** method url {} :transit-json))
  ([method url params-or-content-type]
   (if (keyword? params-or-content-type)
     (base-request** method url {} params-or-content-type)
     (base-request** method url params-or-content-type :transit-json)))
  ([method url params content-type]
   (base-request** method url params content-type)))

(defn req
  "Perform a request with the system's root handler"
  [& args]
  ((handler) (apply base-request args)))

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
  (m/decode muuntaja "application/transit+json" body))

(defmethod read-body "application/json"
  [{:keys [body]}]
  (if (string? body)
    (j/read-value body j/keyword-keys-object-mapper)
    (m/decode muuntaja "application/transit+json" body)))

(defmethod read-body :default
  [{:keys [body]}]
  (if (string? body)
    body
    (slurp body)))
