(ns donut.endpoint.test.harness-test
  (:require [clojure.test :refer [deftest is testing]]
            [donut.endpoint.test.harness :as deth]
            [donut.system :as ds]
            [reitit.core :as rc]))

(defmethod ds/named-system ::test
  [_]
  {::ds/defs
   {:http {:thing "thing"}}})

(deftest with-system-test
  (testing "no custom config"
    (is (= {:http {:thing "thing"}}
           (deth/with-system {:system-name ::test}
             (-> deth/*system*
                 ::ds/instances
                 (dissoc ::deth/config))))))

  (testing "custom config"
    (is (= {:http {:thing "custom thing"}}
           (deth/with-system {:system-name   ::test
                              :custom-config {[:http :thing] "custom thing"}}
             (-> deth/*system*
                 ::ds/instances
                 (dissoc ::deth/config)))))))

(deftest content-type-request-html
  (is (= {:server-port    80
          :server-name    "localhost"
          :remote-addr    "127.0.0.1"
          :protocol       "HTTP/1.1"
          :uri            "/"
          :query-string   ""
          :scheme         :http
          :request-method :get
          :headers        {"host" "localhost"}}
         (deth/content-type-request :get "/" :html)))

  (let [req (deth/content-type-request :post "/" {:x "y"} :html)]
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
  (let [req (deth/content-type-request :get "/" :json)]
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
    (is (= {}
           (deth/read-body req))))

  (let [req (deth/content-type-request :post "/" {:x :y} :json)]
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
  (let [req (deth/content-type-request :get "/" :transit-json)]
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
    (is (= {}
           (deth/read-body req))))

  (let [req (deth/content-type-request :post "/" {:x :y} :transit-json)]
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

  (let [req (deth/content-type-request :post "/" {:x :y} :transit-json)]
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
                                  ["/api/test/:id" ::test-id]])}}})

(deftest path-test
  (deth/with-system {:system-name ::req-test}
    (is (= "/foo"        (deth/path "/foo")))
    (is (= "/api/test"   (deth/path ::test)))
    (is (= "/api/test/1" (deth/path [::test-id {:id 1}])))
    (is (= "/api/test/1?foo=bar+baz" (deth/path [::test-id {:id 1} {:foo "bar baz"}])))))

(deftest handle-request-test
  (deth/with-system {:system-name ::req-test}
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
