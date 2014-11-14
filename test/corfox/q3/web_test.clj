(ns corfox.q3.web-test
  (:use [corfox.q3.web] :reload-all)
  (:use [clojure.test]))

(deftest smoke-test
  (is (= "foo" (inner-html "foo"))))
