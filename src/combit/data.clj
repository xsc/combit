(ns ^{ :doc "Combit Data"
       :author "Yannick Scherer" }
  combit.data
  (:use [combit.utils :as u]))

;; ## CombitData Protocol

(defprotocol CombitData
  "Protocol for all data processable by Combit."
  (set-elements  [this indices values] "Set the elements at the given positions to the given values.")
  (get-elements  [this indices] "Get the elements at the given positions.")
  (element-count [this] "Get the number of elements.")
  (pad-right     [this n] "Add elements at the end of the data.")
  (pad-left      [this n] "Add elements at the beginning of the data."))

(defn set-at
  "Set the element at the given position to the given value."
  [this index value]
  (set-elements this [index] [value]))

(defn get-at
  "Get the element at the given position."
  [this index]
  (first (get-elements this [index])))

;; ## Built-In Implementations

(extend-protocol CombitData
  
  clojure.lang.IPersistentVector
  (set-elements [this indices values]
    (reduce
      (fn [v [index value]]
        (assoc v index value))
      this
      (map vector indices values)))
  (get-elements [this indices]
    (map #(get this %) indices))
  (element-count [this]
    (count this))
  (pad-right [this n]
    (vec (concat this (u/new-vector n nil))))
  (pad-left [this n]
    (vec (concat (u/new-vector n nil) this)))

  clojure.lang.ISeq
  (set-elements [this indices values]
    (seq (set-elements (vec this) indices values)))
  (get-elements [this indices]
    (map #(nth this %) indices))
  (element-count [this]
    (count this))
  (pad-right [this n]
    (concat this (u/new-vector n nil)))
  (pad-left [this n]
    (concat (u/new-vector n nil) this)))

