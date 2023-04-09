(ns donut.endpoint-test.example
  (:require
   [clojure.test :refer [deftest is use-fixtures]]
   [donut.endpoint.router :as der]
   [donut.endpoint.test.harness :as deth]
   [donut.system :as ds]))

(use-fixtures :each (ds/system-fixture :test))

;; donut.endpoint.test.harness is meant to work with systems defined by
;; donut.system, and with systems

(defmethod ds/named-system :test
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
  (is (-> (deth/handle-request :get :books)
          deth/read-body
          (deth/contains-entity? #:book{:id 0 :title "East of Eden"}))))

(deftest get-book
  (is (-> (deth/handle-request :get :book {:book/id 0})
          deth/read-body
          (deth/contains-entity? #:book{:id 0 :title "East of Eden"}))))
