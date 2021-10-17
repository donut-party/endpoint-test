(ns donut.endpoint.test.harness-test
  (:require [clojure.test :refer [deftest is testing]]
            [donut.endpoint.test.harness :as deth]
            [donut.system :as ds]))

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
           (dissoc req :body)))
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
