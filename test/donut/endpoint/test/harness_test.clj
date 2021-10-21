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
             (::ds/instances deth/*system*)))))

  (testing "custom config"
    (is (= {:http {:thing "custom thing"}}
           (deth/with-system [::test {::ds/defs {:http {:thing "custom thing"}}}]
             (::ds/instances deth/*system*))))))

(deftest base-request-html
  (is (= {:server-port    80
          :server-name    "localhost"
          :remote-addr    "127.0.0.1"
          :protocol       "HTTP/1.1"
          :uri            "/"
          :query-string   ""
          :scheme         :http
          :request-method :get
          :headers        {"host" "localhost"}}
         (deth/base-request :get "/" :html)))

  (let [req (deth/base-request :post "/" {:x "y"} :html)]
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

(deftest base-request-json
  (let [req (deth/base-request :get "/" :json)]
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

  (let [req (deth/base-request :post "/" {:x :y} :json)]
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

(deftest base-request-transit
  (let [req (deth/base-request :get "/" :transit-json)]
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
           (dissoc req :body)
           ;; defaults to :transit-json
           (dissoc (deth/base-request :get "/") :body)))
    (is (= {}
           (deth/read-body req))))

  (let [req (deth/base-request :post "/" {:x :y} :transit-json)]
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

(deftest base-request-throws-on-unknown-type
  (is (thrown? java.lang.IllegalArgumentException
               (deth/base-request :get "/" :unsupported))))

(defmethod ds/config ::req-test
  [_]
  {::ds/defs
   {:http {:handler identity
           :router  (rc/router [["/api/test" ::test]])}}})

(deftest req-test
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
           (-> (deth/req :get ::test)
               (dissoc :body))))))
