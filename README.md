[![Clojars Project](https://img.shields.io/clojars/v/party.donut/endpoint-test.svg)](https://clojars.org/party.donut/endpoint-test)

Testing your application's HTTP API endpoint involves coming up with a
consistent ways to:

* Construct endpoint paths
* Send requests to your app's request handler
* Parse responses back into Clojure data structures

This is the kind of day-to-day grind that can turn into an unexpected time sink
and lead to problems down the line if not done well.

This library does all that for you so that your tests can focus as much as
possible on your applciation's business logic and behavior. Instead of writing
tests that look like this:

``` clojure
(deftest get-book
  (is (= (->> (mock/request :get (str "/book/" (:book/id book)))
              handler
              :body
              slurp
              (mu/decode "application/transit+json"))
         #:book{:id 0, :title "East of Eden"})))
```

You can write tests that looks like this:

``` clojure
(deftest get-book
  (is (= (deth/response :get :book {:book/id 0})
         #:book{:id 0 :title "East of Eden"})))
```

endpoint-test takes care of the machinery of constructing requests, handling
them using your application's handler, and reading responses. It's not going to
like, change your life, but it'll make it a little nicer.

## Example

endpoint-test is meant to be used with
[donut.system](https://github.com/donut-party/system), and it relies on the
presence of a ring handler component at `[:http :handler]` a reitit router
component at `[:routing :router]`.

``` clojure
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
  (is (-> (deth/response :get :books)
          (deth/contains-entity? #:book{:id 0 :title "East of Eden"}))))

(deftest get-book
  (is (= (deth/response :get :book {:book/id 0})
         #:book{:id 0 :title "East of Eden"})))
```

## Usage

All examples assume `(require [donut.endpoint.test.harness :as deth])`.

Add this fixture to your tests:

``` clojure
(use-fixtures :each (ds/system-fixture :name-of-your-test-system))
```

This will start a system and bind it to `donut.system/*system*`, which
endpoint-test relies on to get your applications ring handler and router.

If you need to modify your system for your test, you can also use the
`ds/with-*system*` macro:

``` clojure
(deftest get-book
  (ds/with-*system* :test
    (is (= (deth/response :get :book {:book/id 0})
           #:book{:id 0 :title "East of Eden"}))))
```

### Configuring your system

Add `deth/test-harness-plugin` to your system's plugins:

``` clojure
(defmethod ds/named-system :test
  [_]
  {::ds/defs 
   {}
   
   ::ds/plugins
   [deth/test-harness-plugin]})
```

This plugin configures your system, setting some default values that the
library's helpers rely on to make your tests more concise. It tells the library
where to find a router comonent and an http-handler component, and it adds a
component that sets the default content type of requests to `:transit-json`
(other values are `:json` and `:html`).

### Testing requests and responses

endpoint-test offers the following helper functions for creating requests,
handling them, and getting responses:

**`request`**

This constructs a ring request using `ring.mock.request/request`. It lets you
construct a path using a route name, like so:

``` clojure
(request :get :books)             ;; could use the path "/api/v1/books"
(request :get :book {:book/id 1}) ;; could use the path "/api/v1/books/1"
```

It also set's the request's content type to whatever your application's
`:default-request-content-type` is (see below.)

**`handle-request`**

Creates a request using `request` and passes it to the ring handler you've
defined in your system. 

``` clojure
(handle-request :get :books)
```

**`read-body`**

Decodes a response body, traqnsforming it from JSON or whatever back into
Clojure.

``` clojure
(read-body (handle-request :get :books))
```

**`response`**

Composition of `handle-request` and `read-body`

``` clojure
(deftest get-book
  (is (= (deth/response :get :book {:book/id 0})
         #:book{:id 0 :title "East of Eden"})))
```

## Using the plugin

endpoint-test relies on some configuration within your system to retrieve the
components it needs to create request paths with a router and to pass requests
to your ring handler. This configuration is provided by
`deth/test-harness-plugin`:

``` clojure
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
```

The important part is `:donut.system.plugin/system-defaults` at the end.
Whatever values you set in your system map will take precedence over these values.

Change the value of the component `[::deth/config
:default-request-content-type]` to `:json` or `:html` or your own custom
content-type if your endpoints return that content and not transit.

## Development

Run tests

``` sh
clojure -X:test
```
