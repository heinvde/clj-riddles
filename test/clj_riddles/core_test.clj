(ns clj-riddles.core-test
  (:require [clojure.test :refer :all]
            [clj-riddles.core :as core]))

(defn unordered= [vec1 vec2]
  (= (set vec1) (set vec2)))

(deftest test-difference
  (testing "can get difference"
    (is (= [1 3]
           (#'core/difference [1 2 3 4] [6 4 5 2]))))
  (testing "can get no difference"
    (is (= []
           (#'core/difference [1 2 3 4] [4 3 2 1]))))
  (testing "can handle empty left vector"
    (is (= []
           (#'core/difference [] [4 3 2 1]))))
  (testing "can handle empty right vector"
    (is (unordered= [4 3 2 1]
                    (#'core/difference [4 3 2 1] []))))
  (testing "can handle multiple values"
    (is (unordered= [1]
                    (#'core/difference [1 1 2 2] [1 2 2])))))

(deftest test-get-shipment-boxes
  (testing "can get boxes for shipment"
    (is (= [6 2]
           (#'core/get-shipment-boxes 10 [6 2 4 5]))))
  (testing "can get no boxes for shipment"
    (is (= []
           (#'core/get-shipment-boxes 2 [6 3 4 5]))))
  (testing "can get exact boxes for shipment"
    (is (= [1 1 2 3]
           (#'core/get-shipment-boxes 7 [1 1 2 3]))))
  (testing "can get trip count for large trucks and boxes"
    (is (= [10 2 16]
           (#'core/get-shipment-boxes 29 [10 2 16 19])))))

(deftest test-get-next-trip
  (testing "can get trip shipments"
    (is (= {:shipments [{:truck 1, :boxes [2 3]} {:truck 2, :boxes [3]}]}
           (#'core/get-next-trip [[1 5] [2 3]] [2 3 3]))))
  (testing "can get trip shipments with extra"
    (is (= {:shipments [{:truck 1, :boxes [2 3]} {:truck 2, :boxes [3]}]}
           (#'core/get-next-trip [[1 5] [2 3]] [2 3 3 3]))))
  (testing "can get trip shipments with one truck too small"
    (is (= {:shipments [{:truck 1, :boxes [2 3]}]}
           (#'core/get-next-trip [[1 5] [2 2]] [2 3 3 3])))))

(deftest test-get-shipments
  (testing "can get shipments for one truck one boxes"
    (is (unordered= [{:truck 1 :boxes [7]}]
                    (#'core/get-shipments [10] [7]))))
  (testing "can get shipments for one truck with exact capacity"
    (is (unordered= [{:truck 1 :boxes [5 3 2]}]
                    (#'core/get-shipments [10] [5 3 2]))))
  (testing "can get shipments for one truck multiple boxes"
    (is (unordered= [{:truck 1 :boxes [7 2]} {:truck 1 :boxes [4]}]
                    (#'core/get-shipments [10] [7 4 2]))))
  (testing "can get shipments for multiple trucks and boxes"
    (is (unordered= [{:truck 1 :boxes [4]} {:truck 2 :boxes [7 2]}]
                    (#'core/get-shipments [4 10] [7 4 2]))))
  (testing "can get shipments for multiple trucks and boxes with multiple trips"
    (is (unordered= [{:truck 2 :boxes [7 3]} {:truck 2 :boxes [6 4]} {:truck 2 :boxes [5]}]
                    (#'core/get-shipments [3 10] [7 6 5 4 3]))))
  (testing "can get trip count for large trucks and boxes"
    (is (unordered= [{:truck 1 :boxes [10 2 16]} {:truck 2 :boxes [19]}]
                    (#'core/get-shipments [29 25] [10 2 16 19])))))

(deftest test-get-trip-estimate
  (testing "can get trip count for one truck one box"
    (is (= 1 (core/get-trip-estimate [10] [7]))))
  (testing "can get trip count for one truck two box with return trips"
    (is (= 3 (core/get-trip-estimate [10] [7 6]))))
  (testing "can get trip count for multiple trucks and boxes"
    (is (= 5 (core/get-trip-estimate [10 3] [7 6 5 4 3]))))
  (testing "can get trip count for large trucks and boxes"
    (is (= 2 (core/get-trip-estimate [29 25] [10 2 16 19])))))

