(ns corfox.q3.stats-test
  (:use [corfox.q3.stats] :reload-all)
  (:use [clojure.test]))

(deftest smoke-test
  (is (contains? player-names "Adam")))
