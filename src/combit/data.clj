(ns ^{ :doc "Combit Data"
       :author "Yannick Scherer" }
  combit.data
  (:use [combit.utils :as u]))

;; ## CombitData Protocol

(defprotocol CombitData
  "Protocol for all data processable by Combit."
  (set-elements  [this indices values] 
    "Set the elements at the given positions to the given values.")
  (get-elements  [this indices]
    "Create new data block consisting of the elements at the given positions.")
  (remove-elements [this indices]
    "Create new data block consisting of all elements expect the ones at the given indices.
     Indices might be rearranged afterwards.")
  (concat-elements [this values]
    "Append the given seq of values to this data block.")
  (element-count [this] 
    "Get the number of elements.")
  (data-seq [this]
    "Create lazy seq of elements."))

(defn empty-data?
  "Returns true if the data contains no elements."
  [this]
  (not (data-seq this)))

(defn set-at
  "Set the element at the given position to the given value."
  [this index value]
  (set-elements this [index] [value]))

(defn get-at
  "Get the element at the given position."
  [this index]
  (first (data-seq (get-elements this [index]))))

(defn remove-at
  "Remove the element at the given position."
  [this index]
  (remove-elements this [index]))

(defn concat-data
  "Concatenate the given data blocks."
  [& ds]
  (reduce
    (fn [data d]
      (concat-elements data (data-seq d)))
    ds))

;; ### Built-In Implementations

(extend-type clojure.lang.IPersistentVector
  CombitData
  (set-elements [this indices values]
    (vec 
      (reduce
        (fn [v [index value]]
          (assoc v index value))
        this
        (map vector indices values))))
  (get-elements [this indices]
    (vec (map #(get this %) indices)))
  (remove-elements [this indices]
    (vec (remove-elements (seq this) indices)))
  (concat-elements [this values]
    (vec (concat this values)))
  (element-count [this]
    (count this))
  (data-seq [this]
    (seq this)))

(extend-type clojure.lang.ISeq
  CombitData
  (set-elements [this indices values]
    (seq (set-elements (vec this) indices values)))
  (get-elements [this indices]
    (map #(nth this %) indices))
  (remove-elements [this indices]
    (->> 
      (map vector this (range))
      (filter (comp not (set indices) second))
      (map first)))
  (concat-elements [this values]
    (concat this values))
  (element-count [this]
    (count this))
  (data-seq [this]
    this))

;; ## Take/Drop
;;
;; `take-elements` and `drop-elements` can be implemented using `get-elements` and 
;; `remove-elements`, but to enable more efficient implementations for single types,
;; they are designed as multimethods. Take and Drop operations are, e.g., used when
;; a stream component splits its input into data usable by block-components.

(defmulti take-elements
  "Create new data consisting of the first n elements."
  (fn [n data]
    (class data))
  :default nil)

(defmethod take-elements nil
  [n this]
  (get-elements this (range n)))

(defmulti drop-elements
  "Create new data consisting of everything except the first n elements."
  (fn [n data]
    (class data))
  :default nil)

(defmethod drop-elements nil
  [n this]
  (remove-elements this (range n)))

;; ### Built-In Implementations

(defmethod take-elements clojure.lang.IPersistentVector
  [n this]
  (vec (take n this)))

(defmethod drop-elements clojure.lang.IPersistentVector
  [n this]
  (vec (drop n this)))

(defmethod take-elements clojure.lang.ISeq
  [n this]
  (take n this))

(defmethod drop-elements clojure.lang.ISeq
  [n this]
  (drop n this))
