(ns donut.endpoint.test.harness
  "Includes:

  * Macros and a fixture for dealing with a system for the duration of a test
  * Helpers for composing and dispatching requests
  * `read-body` multimethod for parsing response bodies of different types (transit, json etc)
  * assertions that work with response segments"
  (:require
   [com.rpl.specter :as specter]
   [donut.system :as ds]
   [malli.core :as m]
   [meta-merge.core :as mm]
   [muuntaja.core :as mu]
   [reitit.core :as rc]
   [ring.mock.request :as mock]
   [ring.util.codec :as ring-codec]))

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
  [config & body]
  `(let [[config-name# custom-config#] ~config
         conf# (-> (ds/config config-name#)
                   (update-in [::ds/defs ::config]
                              #(mm/meta-merge ConfigurationComponentGroup %)))]
     (binding [*system* (ds/start conf# custom-config#)]
       (let [return# (do ~@body)]
         (ds/stop *system*)
         return#))))

(defn system-fixture
  "To be used with `use-fixtures`"
  [config]
  (fn [f]
    (with-system config (f))))

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

(defn route-path
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

(defprotocol Path
  (path [this]))

(extend-protocol Path
  String
  (path [this] this)

  clojure.lang.PersistentVector
  (path [[route-name route-params query-params]]
    (route-path route-name route-params query-params))

  clojure.lang.Keyword
  (path [this] (route-path this)))


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


(defmulti content-type-request*
  (fn [_method _url _params content-type]
    content-type))

(defmethod content-type-request* :transit-json
  [method url params _]
  (-> (mock/request method url)
      (headers {:content-type "application/transit+json"
                :accept       "application/transit+json"})
      (assoc :body (mu/encode "application/transit+json" params))))

(defmethod content-type-request* :json
  [method url params _]
  (-> (mock/request method url)
      (headers {:content-type "application/json"
                :accept       "application/json"})
      (assoc :body (mu/encode "application/json" params))))

(defmethod content-type-request* :html
  [method url params _]
  (mock/request method url params))

(defn content-type-request
  ([method path-or-route-name content-type]
   (content-type-request method path-or-route-name {} content-type))
  ([method path-or-route-name params content-type]
   (content-type-request* method
                          (path path-or-route-name)
                          params
                          content-type)))

(defn request
  ([method path-or-route-name]
   (content-type-request method
                         path-or-route-name
                         {}
                         (configured-value :default-request-content-type)))
  ([method path-or-route-name params-or-content-type]
   (if (keyword? params-or-content-type)
     (content-type-request method
                           path-or-route-name
                           {}
                           params-or-content-type)
     (content-type-request method
                           path-or-route-name
                           params-or-content-type
                           (configured-value :default-request-content-type))))
  ([method path-or-route-name params content-type]
   (content-type-request* method path-or-route-name params content-type)))

(defn handle-request
  "Perform a request with the system's root handler"
  [method path-or-route-name & args]
  ((handler) (apply request method path-or-route-name args)))

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
  (mu/decode "application/transit+json" body))

(defmethod read-body "application/json"
  [{:keys [body]}]
  (mu/decode "application/json" body))

(defmethod read-body :default
  [{:keys [body]}]
  (if (string? body)
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
      :segments (specter/select [specter/ALL
                                 #(= (first %) :entities)
                                 1
                                 2
                                 specter/ALL]
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
  "Request's response data creates entity of type `ent-type` that has
  key/value pairs identical to `test-ent-attrs`."
  [resp-data test-ent-attrs]
  ((comparison-entities test-ent-attrs resp-data) test-ent-attrs))
