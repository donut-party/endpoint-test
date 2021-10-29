(ns donut.endpoint.test.harness-test
  (:require [clojure.test :refer [deftest is testing]]
            [donut.endpoint.test.harness :as deth]
            [donut.system :as ds]
            [reitit.core :as rc]))

(defmethod ds/config ::test
  [_]
  {::ds/defs
   {:http {:thing "thing"}}})

(deftest with-system-test
  (testing "no custom config"
    (is (= {:http {:thing "thing"}}
           (deth/with-system [::test]
             (-> deth/*system*
                 ::ds/instances
                 (dissoc ::deth/config))))))

  (testing "custom config"
    (is (= {:http {:thing "custom thing"}}
           (deth/with-system [::test {::ds/defs {:http {:thing "custom thing"}}}]
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

(deftest content-type-request-throws-on-unknown-type
  (is (thrown? java.lang.IllegalArgumentException
               (deth/content-type-request :get "/" :unsupported))))

(defmethod ds/config ::req-test
  [_]
  {::ds/defs
   {:http {:handler identity
           :router  (rc/router [["/api/test" ::test]
                                ["/api/test/:id" ::test-id]])}}})

(deftest path-test
  (deth/with-system [::req-test]
    (is (= "/foo"        (deth/path "/foo")))
    (is (= "/api/test"   (deth/path ::test)))
    (is (= "/api/test/1" (deth/path [::test-id {:id 1}])))))

(deftest handle-request-test
  (deth/with-system [::req-test]
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
