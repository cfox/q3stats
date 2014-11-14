(ns corfox.q3.views-test
  (:use [corfox.q3.views] :reload-all)
  (:use [clojure.test]))

(deftest smoke-test
  (is (> (.indexOf (name-link "foo")
                   "foo")
         0)))
