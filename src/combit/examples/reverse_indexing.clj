(ns ^{ :doc "Example of Container Wrapper where last element has Index 0."
       :author "Yannick Scherer" }
  combit.examples.reverse-indexing
  (:use combit.data
        combit.core))

;; ## Helpers

(defn- adjust-index
  "Reverse Index."
  [data i]
  (- (element-count data) i 1))

(defn- adjust-indices
  "Reverse Indices."
  [data is]
  (map #(adjust-index data %) is))

;; ## Type with Reverse Indexing

(deftype RVec [data]
  CombitData
  (element-count [this]
    (element-count data))
  (get-at [this index]
    (get-at data (adjust-index data index)))
  (get-elements [this indices]
    (RVec. 
      (get-elements data (adjust-indices data indices))))
  (set-at [this index value]
    (RVec. 
      (set-at data (adjust-index data index) value)))
  (set-elements [this indices values]
    (RVec.
      (set-elements data (adjust-indices data indices) values)))
  (remove-elements [this indices]
    (RVec.
      (remove-elements data (adjust-indices data indices))))
  (concat-elements [this values]
    (RVec.
      (concat-elements data values)))
  (data-seq [this]
    (data-seq data)))

(defn rvec
  "Wrap given data into reverse indexing wrapper."
  [data]
  (RVec. data))

(defn rvector
  "Create reverse-indexed Vector."
  [& args]
  (rvec (vec args)))

;; ## Components using Reverse Indexing

(def-component rswapper [in 4] [upper (rvector 0 0) lower (rvector 0 0)]
  (>>* (in 1 0) (lower 1 0))
  (>>* (in 3 2) (upper 1 0)))

(defn -main
  [& x]
  (->> x
    (take 4)
    (vec)
    (rvec)
    (rswapper)
    (map data-seq)
    println))
