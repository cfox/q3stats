(ns corfox.q3.ofchart-test
  (:use [corfox.q3.ofchart] :reload-all)
  (:use [clojure.test]))

(deftest smoke-test
  (is (> (.indexOf (ofchart "foo")
                   "foo")
         0))
  (is (> (.indexOf (ofchart-data ["foo"] ["bar"] ["baz"])
                   "foo")
         0)))

