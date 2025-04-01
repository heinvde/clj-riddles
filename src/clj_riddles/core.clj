(ns clj-riddles.core
  (:require [clojure.set]))

;; ===== Truck loading riddle =====
;; You are given a set of boxes, each with a specific weight, that need to be transported to another location.
;; You have multiple trucks available, each with a fixed weight capacity.
;; A truck can carry multiple boxes as long as their total weight does not exceed its capacity.
;; The journey to the destination takes 1 hour.
;; If a truck cannot carry all its assigned boxes in one trip, it must return (1 hour) and make additional trips.
;;
;; === Question: What is the total time all the trucks will spend traveling?

(defn- remove-first
  "Removes the first occurance of an item"
  ([from-seq val]
   (remove-first from-seq val 0))
  ([from-seq val index]
   (cond
     ; exit
     (<= (count from-seq) index) from-seq
     ; remove val
     (= val (nth from-seq index)) (concat (take index from-seq) (drop (inc index) from-seq))
     ; check next
     :else (recur from-seq val (inc index)))))

(defn- difference
  "Returns the items from vec-1 that is not in vec-2."
  [vec-1 vec-2]
  (reduce remove-first vec-1 vec-2))

(defn- get-shipment-boxes
  "Returns all the boxes that can be loaded into a truck."
  [truck-capacity boxes]
  (letfn [(get-truck-capacity
           [packed-boxes] (->> packed-boxes (reduce +) (- truck-capacity)))
          (try-fit-box
           [packed-boxes box]
           (if (> box (get-truck-capacity packed-boxes))
             packed-boxes
             (conj packed-boxes box)))]
    (reduce try-fit-box [] boxes)))

(defn- get-next-trip
  "Returns the shipments for the next trip to the destination. Returns: {:shipments [{:truck :boxes}]}."
  [trucks boxes]
  (letfn [(has-more-load?
           [truck1 truck2] (> (-> truck1 :boxes count)
                              (-> truck2 :boxes count)))
          (get-truck-shipment
           [[truck capacity] boxes] {:truck truck
                                     :boxes (get-shipment-boxes capacity boxes)})
          (has-boxes
           [truck] (-> truck :boxes count zero? not))]
    (loop [remaining-trucks trucks ;[ [name capacity] ...]
           remaining-boxes boxes
           shipments []]
      (if-some [best-shipment (->>
                               remaining-trucks
                               (map #(get-truck-shipment % remaining-boxes))
                               (filter has-boxes)
                               (sort has-more-load?)
                               (first))] ; Choose best shipment (truck that takes most boxes)
        (recur
         (filter #(not= (first %) (:truck best-shipment)) remaining-trucks) ; Remove best truck
         (->> best-shipment :boxes (difference remaining-boxes)) ; Remove packed boxes
         (conj shipments best-shipment))
        {:shipments shipments}))))

(defn- get-shipments 
  "Returns array of shipments required for the transfer. Returns: [{:truck :boxes} ...]."
  [trucks boxes]
  (loop [remaining-boxes boxes
         shipments []]
    (let [fmt-truck (fn [index capacity] (vec [(inc index) capacity]))
          trip-shipments (as-> trucks $ 
                           (map-indexed fmt-truck $) ; [[truck-index truck-capacity] ...]
                           (get-next-trip $ remaining-boxes)
                           (:shipments $))
          get-remaining-boxes (comp
                               (partial difference remaining-boxes)
                               flatten
                               (partial map :boxes))]
      (if (empty? trip-shipments)
        (vec shipments)
        (recur
         (get-remaining-boxes trip-shipments)
         (concat shipments trip-shipments))))))

(def transfer-time 1)
(def return-time transfer-time)

(defn get-trip-estimate
  "Takes an array of truck capacities and an array of box weights and returns the estimated total travel time in hours required to transport all boxes to the destination."
  [trucks boxes]
  (letfn [(travel-time 
           [transfer-count]
           (+ (* transfer-time transfer-count)
              (* return-time (dec transfer-count))))]
    (->> (get-shipments trucks boxes) ; [{:truck :boxes} ...]
         (sort-by :truck)
         (partition-by :truck)
         (map count) ; transfers per truck
         (map travel-time) ; travel time per truck
         (reduce +))))

