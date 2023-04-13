(ns donut.endpoint.test.harness
  "Includes:

  * Macros and a fixture for dealing with a system for the duration of a test
  * Helpers for composing and dispatching requests
  * `read-body` multimethod for parsing response bodies of different types (transit, json etc)
  * assertions that work with response segments"
  (:require
   [donut.system :as ds]
   [malli.core :as m]
   [muuntaja.core :as mu]
   [reitit.core :as rc]
   [ring.mock.request :as mock]
   [ring.util.codec :as ring-codec]))

(def Method
  [:method #{:get :head :post :put :delete :connect :options :trace :patch}])

(def RouteIdentifier)

(def BaseRequest
  [:map
   [:method [:enum :get :head :post :put :delete :connect :options :trace :patch]]
   [:body-params {:optional true} map?]
   [:query-params {:optional true} map?]
   [:content-type {:optional true} keyword?]])

(def Request
  [:or
   (into BaseRequest [[:route-name  keyword?]
                      [:route-params {:optional true} map?]])
   (into BaseRequest [[:path string?]])])

(def test-harness-plugin
  {:donut.system.plugin/name
   ::test-harness-plugin

   :donut.system.plugin/doc
   "Configures system so that donut.endpoint.test.harness can find the
   components needed to construct and dispatch requests."

   :donut.system.plugin/system-defaults
   {::ds/registry {:donut/endpoint-router [:routing :router]
                   :donut/http-handler    [:http :handler]}
    ::ds/defs     {::config {:default-request-content-type :transit-json}}}})

;; -------------------------
;; system wrapper helpers
;; -------------------------

(defn system
  "Get ds/*system* and throw when it's not set. Used by other helpers. Intention
  is to reduce the boilerplate of system retrieval in your tests."
  []
  (when-not ds/*system*
    (throw (ex-info "donut.system/*system* is nil but should be a system.
Try adding (use-fixtures :each (ds/system-fixture :test-system-name)) to your test namespace." {})))
  ds/*system*)

(defn configured-value
  "Look up a component instance that has path configured by the `::config`
  component group"
  [config-key]
  (or (get-in (system) [::ds/instances ::config config-key])
      (throw (ex-info "Missing test configuration value"
                      {:system-path [::ds/defs ::config config-key]}))))

;; ---
;; paths
;; ---

(defn router
  "Retrieve endpoint router from ds/*system*. Relies on `:donut/endpoint-router`
  being set in the system's `::ds/registry`"
  []
  (ds/registry-instance (system) :donut/endpoint-router))

(defn route-path
  "Construct a path like /api/v1/books/1. Can also build query string."
  ([route-name]
   (route-path route-name {} {}))
  ([route-name route-params]
   (route-path route-name route-params {}))
  ([route-name route-params query-params]
   ;; TODO assert that router is a router
   (let [{:keys [path]} (rc/match-by-name (router) route-name route-params)]
     (if path
       (cond-> path
         (not-empty query-params) (str "?" (ring-codec/form-encode query-params)))
       (throw (ex-info "could not generate router path" {:route-name   route-name
                                                         :route-params route-params}))))))

;; -------------------------
;; compose and dispatch requests
;; -------------------------

(defn handler
  "Retrieve endpoint handler from ds/*system*. Relies on `:donut/http-handler`
  being set in the system's `::ds/registry`"
  []
  (ds/registry-instance (system) :donut/http-handler))

(defn headers
  "Add all headers to request"
  [req headers]
  (reduce-kv mock/header req headers))


(defmulti content-type-request
  "Multi method that sets headers and encodes body for a content type"
  (fn [{:keys [content-type]}]
    content-type))

(defmethod content-type-request :transit-json
  [{:keys [method path body-params]}]
  (-> (mock/request method path)
      (headers {:content-type "application/transit+json"
                :accept       "application/transit+json"})
      (assoc :body (mu/encode "application/transit+json" body-params))))

(defmethod content-type-request :json
  [{:keys [method path body-params]}]
  (-> (mock/request method path)
      (headers {:content-type "application/json"
                :accept       "application/json"})
      (assoc :body (mu/encode "application/json" body-params))))

(defmethod content-type-request :edn
  [{:keys [method path body-params]}]
  (-> (mock/request method path)
      (headers {:content-type "application/edn"
                :accept       "application/edn"})
      (assoc :body (mu/encode "application/edn" body-params))))

(defmethod content-type-request :html
  [{:keys [method path body-params]}]
  (mock/request method path body-params))

(defn request
  "Construct a ring.mock.request/request using your ds/*system* router to
   build a path. Adds content-type and accept headers.

   Single-arity version takes a map with:

         :method HTTP method
           :path string; use if you don't want to use router to build path
     :route-name used to build path
   :route-params map to populate your route's params,
                 e.g. :book/id in /api/books {book/id}
   :query-params map to add a query string
   :body-params  the data for your request"
  ([{:keys [path route-name route-params query-params content-type] :as req-map}]
   (-> req-map
       (assoc :content-type (or content-type (configured-value :default-request-content-type))
              :path (or path (route-path route-name route-params query-params)))
       content-type-request))
  ([method path-or-route-name & [route-params body-params query-params]]
   (request (cond-> {:method method
                     :body-params body-params
                     :route-params route-params
                     :query-params query-params}
              (string? path-or-route-name)  (assoc :path path-or-route-name)
              (keyword? path-or-route-name) (assoc :route-name path-or-route-name)))))

#_:clj-kondo/ignore
(defn handle-request
  "Perform a request with the system's ring handler"
  ([{:keys [path route-name route-params body-params query-params content-type] :as req-map}]
   ((handler) req-map))
  ([method path-or-route-name & [route-params body-params query-params]]
   ((handler) (request method path-or-route-name route-params body-params query-params))))

;; -------------------------
;; read responses
;; -------------------------

(defmulti read-body
  "Decodes a response body, transforming it from JSON or whatever back into
  Clojure, which we love"
  (fn [{:keys [headers]}]
    (->> (or (get headers "Content-Type")
             (get headers "content-type")
             "")
         (re-matches #"(.*?)(;.*)?")
         second)))

(defmethod read-body "application/transit+json"
  [{:keys [body]}]
  (mu/decode "application/transit+json" body))

(defmethod read-body "application/json"
  [{:keys [body]}]
  (mu/decode "application/json" body))

(defmethod read-body :default
  [{:keys [body]}]
  (if (or (map? body)
          (vector? body)
          (set? body)
          (string? body))
    body
    (slurp body)))

;; -------------------------
;; response data predicates
;; -------------------------

(def Entity map?)
(def Entities [:vector Entity])
(def Segment [:tuple keyword? any?])
(def Segments [:vector Segment])
(def Response
  [:orn
   [:entity Entity]
   [:entities Entities]
   [:segments Segments]])

(defn response-entities
  "Walk response data and return all entities from entity segments"
  [resp-data]
  (let [[response-data-type _] (m/parse Response resp-data)]
    (case response-data-type
      :entity   [resp-data]
      :entities resp-data
      :segments (into []
                      (comp (filter #(= (first %) :entities))
                            (map second)
                            (map #(drop 2 %))
                            cat
                            cat)
                      resp-data))))

(defn prep-comparison
  "When testing whether a response contains `test-ent-attrs`, we modify
  a response entity by:

  1. selecting only the keys that are present in
     `test-ent-attrs`. This allows us to do an `=` comparison that won't
     fail if the response entity contains attributes we don't want to
     test.

  2. Putting the result in a map to handle case where `resp-entity` is
     a record."
  [resp-entity test-ent-attrs]
  (into {} (select-keys resp-entity (keys test-ent-attrs))))

(defn comparison-entities
  "Returns a set that can be used to test if `test-ent-attrs` is
  contained in a response"
  [test-ent-attrs resp-data]
  (->> resp-data
       (response-entities)
       (map #(prep-comparison % test-ent-attrs))
       (set)))

(defn contains-entity?
  "Request's response data has a map with key/value pairs identical to
  `test-ent-attrs`."
  [resp-data test-ent-attrs]
  ((comparison-entities test-ent-attrs resp-data) test-ent-attrs))

#_:clj-kondo/ignore
(defn response
  "Comp of handle-request and read-body; handles a request and reads the body."
  ([{:keys [path route-name route-params body-params query-params content-type] :as req-map}]
   (read-body (handle-request req-map)))
  ([method path-or-route-name & [route-params body-params query-params]]
   (read-body (handle-request method path-or-route-name route-params body-params query-params))))
