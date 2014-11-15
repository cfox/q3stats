(ns corfox.q3.charts-test
  (:use [corfox.q3.charts] :reload-all)
  (:use [clojure.test]))

(deftest smoke-test
  (is (= "Corbin" (:player-name (extract-player {:player "Boom King"})))))

(deftest chart-test
  (let [chart-data (chart {:chartname "player-kills-by-weapon"})]
    (print chart-data)
    ))
