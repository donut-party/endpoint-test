(ns donut.endpoint.test.harness-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [donut.endpoint.router :as der]
   [donut.endpoint.test.harness :as deth]
   [donut.system :as ds]
   [reitit.core :as rc]))

(defmethod ds/named-system ::test
  [_]
  {::ds/defs {:http {:thing "thing"}}
   ::ds/plugins [deth/test-harness-plugin]})

(deftest with-system-test
  (testing "no custom config"
    (is (= {:http {:thing "thing"}}
           (ds/with-*system* ::test
             (-> ds/*system*
                 ::ds/instances
                 (dissoc ::deth/config))))))

  (testing "custom config"
    (is (= {:http {:thing "custom thing"}}
           (ds/with-*system*
             (ds/system ::test {[:http :thing] "custom thing"})
             (-> ds/*system*
                 ::ds/instances
                 (dissoc ::deth/config)))))))

(deftest content-type-request-html
  (is (= {:server-port    80
          :server-name    "localhost"
          :remote-addr    "127.0.0.1"
          :protocol       "HTTP/1.1"
          :uri            "/"
          :scheme         :http
          :request-method :get
          :headers        {"host" "localhost"}}
         (deth/content-type-request
          {:method       :get
           :path         "/"
           :content-type :html})))

  (let [req (deth/content-type-request
             {:method       :post
              :path         "/"
              :body-params  {:x "y"}
              :content-type :html})]
    (is (= {:remote-addr    "127.0.0.1"
            :protocol       "HTTP/1.1"
            :headers        {"host"           "localhost"
                             "content-type"   "application/x-www-form-urlencoded"
                             "content-length" "3"}
            :server-port    80
            :content-length 3
            :content-type   "application/x-www-form-urlencoded"
            :uri            "/"
            :server-name    "localhost"
            :scheme         :http
            :request-method :post}
           (dissoc req :body)))

    (is (= "x=y" (deth/read-body req)))))

(deftest content-type-request-json
  (let [req (deth/content-type-request
             {:method       :get
              :path         "/"
              :content-type :json})]
    (is (= {:remote-addr    "127.0.0.1"
            :protocol       "HTTP/1.1"
            :headers        {"host"         "localhost"
                             "content-type" "application/json"
                             "accept"       "application/json"}
            :server-port    80
            :uri            "/"
            :server-name    "localhost"
            :scheme         :http
            :request-method :get}
           (dissoc req :body)))
    (is (nil? (deth/read-body req))))

  (let [req (deth/content-type-request
             {:method       :post
              :path         "/"
              :body-params  {:x :y}
              :content-type :json})]
    (is (= {:remote-addr    "127.0.0.1"
            :protocol       "HTTP/1.1"
            :headers        {"host"         "localhost"
                             "content-type" "application/json"
                             "accept"       "application/json"}
            :server-port    80
            :uri            "/"
            :server-name    "localhost"
            :scheme         :http
            :request-method :post}
           (dissoc req :body)))
    (is (= {:x "y"}
           (deth/read-body req)))))

(deftest content-type-request-transit-json
  (let [req (deth/content-type-request
             {:method       :get
              :path         "/"
              :content-type :transit-json})]
    (is (= {:remote-addr    "127.0.0.1"
            :protocol       "HTTP/1.1"
            :headers        {"host"         "localhost"
                             "content-type" "application/transit+json"
                             "accept"       "application/transit+json"}
            :server-port    80
            :uri            "/"
            :server-name    "localhost"
            :scheme         :http
            :request-method :get}
           (dissoc req :body)))
    (is (nil? (deth/read-body req))))

  (let [req (deth/content-type-request
             {:method       :post
              :path         "/"
              :body-params  {:x :y}
              :content-type :transit-json})]
    (is (= {:remote-addr    "127.0.0.1"
            :protocol       "HTTP/1.1"
            :headers        {"host"         "localhost"
                             "content-type" "application/transit+json"
                             "accept"       "application/transit+json"}
            :server-port    80
            :uri            "/"
            :server-name    "localhost"
            :scheme         :http
            :request-method :post}
           (dissoc req :body)))
    (is (= {:x :y}
           (deth/read-body req))))

  (let [req (deth/content-type-request
             {:method       :post
              :path         "/"
              :body-params  {:x :y}
              :content-type :transit-json})]
    (is (= {:remote-addr    "127.0.0.1"
            :protocol       "HTTP/1.1"
            :headers        {"host"         "localhost"
                             "content-type" "application/transit+json"
                             "accept"       "application/transit+json"}
            :server-port    80
            :uri            "/"
            :server-name    "localhost"
            :scheme         :http
            :request-method :post}
           (dissoc req :body)))
    (is (= {:x :y}
           (deth/read-body req)))))

(deftest content-type-request-throws-on-unknown-type
  (is (thrown? java.lang.IllegalArgumentException
               (deth/content-type-request :get "/" :unsupported))))

(defmethod ds/named-system ::req-test
  [_]
  {::ds/defs
   {:http    {:handler identity}
    :routing {:router (rc/router [["/api/test" ::test]
                                  ["/api/test/:id" ::test-id]])}}

   ::ds/plugins
   [deth/test-harness-plugin]})

(deftest handle-request-test
  (ds/with-*system* ::req-test
    (is (= {:protocol       "HTTP/1.1"
            :remote-addr    "127.0.0.1"
            :headers        {"host"         "localhost"
                             "content-type" "application/transit+json"
                             "accept"       "application/transit+json"}
            :server-port    80
            :uri            "/api/test"
            :server-name    "localhost"
            :scheme         :http
            :request-method :get}
           (-> (deth/handle-request :get ::test)
               (dissoc :body))))))

;; -------------------------
;; response data predicates
;; -------------------------

(deftest response-entities-one-test
  (is (= [{:foo :bar}]
         (deth/response-entities {:foo :bar}))))

(deftest response-entities-many-test
  (is (= [{:foo :bar} {:boop :moop}]
         (deth/response-entities [{:foo :bar} {:boop :moop}]))))

(deftest response-entities-segments-test
  (is (= [{:a :a} {:b :b} {:c :c} {:d :d}]
         (deth/response-entities [[:entities [:entity-type :id [{:a :a} {:b :b}]]]
                                  [:segment nil]
                                  [:entities [:entity-type :id [{:c :c} {:d :d}]]]]))))

(deftest contains-entity?-test
  (is (deth/contains-entity? {:a :a :b :b}
                             {:a :a}))
  (is (not (deth/contains-entity? {:a :a :b :b}
                                  {:a :a :c :c})))
  (is (deth/contains-entity? [{:a :a :b :b}]
                             {:a :a}))
  (is (deth/contains-entity? [[:entities [:_ :_ [{:a :a}]]]]
                             {:a :a}))
  (is (not (deth/contains-entity? [[:entities [:_ :_ [{:a :a}]]]]

                                  {:a :a :b :b}))))
;;---
;;
;;---


;; donut.endpoint.test.harness is meant to work with systems defined by
;; donut.system, and with systems

(defmethod ds/named-system :with-routes
  [_]
  {::ds/defs
   {:http
    {:handler
     (ds/ref [:routing :ring-handler])}

    :routing
    {:ring-handler der/RingHandlerComponent
     :router       der/RouterComponent
     :router-opts  der/router-opts
     :routes       [;; returning multiple maps
                    ["/api/v1/books"
                     {:name :books
                      :get  {:handler (fn [_]
                                        {:status 200
                                         :body   [#:book{:id 0 :title "East of Eden"}
                                                  #:book{:id 1 :title "The Martian"}
                                                  #:book{:id 2 :title "Their Eyes Were Watching God"}]})}}]

                    ;; returning a single map
                    ["/api/v1/books/{book/id}"
                     {:name :book
                      :get  {:handler (fn [_]
                                        {:status 200
                                         :body   #:book{:id 0 :title "East of Eden"}})}
                      :put  {:handler (fn [{:keys [all-params]}]
                                        all-params)}}]]}}

   ::ds/plugins
   [deth/test-harness-plugin]})

(deftest get-books
  (ds/with-*system* :with-routes
    (is (-> (deth/response :get :books)
            (deth/contains-entity? #:book{:id 0 :title "East of Eden"})))))

(deftest get-book
  (ds/with-*system* :with-routes
    (is (= (deth/response :get :book {:book/id 0})
           #:book{:id 0 :title "East of Eden"}))))
