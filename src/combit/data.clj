(ns ^{ :doc "Combit Data"
       :author "Yannick Scherer" }
  combit.data
  (:use [combit.utils :as u]))

;; ## CombitData

(defprotocol CombitData
  "Protocol for Combit Data Containers."
  (element-count [this]
    "Get number of elements in data object.")
  (get-at [this index]
    "Get element at the given index.")
  (get-elements [this indices]
    "Create object representing the data at the given indices and in the given
     order.")
  (set-at [this index value]
    "Set element at the given index to the given value.")
  (set-elements [this indices values]
    "Create object representing the alterations to a data object specified by the 
     given indices and new element values.")
  (remove-elements [this indices]
    "Create object representing the removal of the elements at the given indices.")
  (concat-elements [this values]
    "Create object representing the concatenation of a data object and a seq of
     element values.")
  (data-seq [this]
    "Get seq of data elements."))

(defn remove-at
  "Remove the element at the given position."
  [data index]
  (remove-elements data [index]))

(defn take-elements
  "Create new data consisting of the elements with indices 0 to n-1."
  [n data]
  (get-elements data (range n)))

(defn drop-elements
  "Create new data consisting of the elements with indices n to N."
  [n data]
  (remove-elements data (range n)))

;; ## Vector Implementation of CombitData

(extend-type clojure.lang.IPersistentVector 
  CombitData
  (element-count [this]
    (count this))
  (get-at [this index]
    (get this index))
  (get-elements [this indices]
    (vec (map #(get this %) indices)))
  (set-at [this index value]
    (assoc this index value))
  (set-elements [this indices values]
    (let [c (count this)
          vc (transient this)]
      (persistent!
        (reduce
          (fn [vc [i v]]
            (assoc! vc i v))
          vc
          (map-indexed #(vector %2 (get-at values %1)) indices)))))
  (remove-elements [this indices]
    (let [c (count this)
          remove-element? (set indices)
          vc (transient [])]
      (persistent!
        (reduce
          (fn [vc [i e]]
            (if (remove-element? i)
              vc
              (conj! vc e)))
          vc
          (map vector (range) this)))))
  (concat-elements [this values]
    (if-not (coll? values)
      (u/throw-error "concat-elements", "expected collection to append, given: " values)
      (let [vc (transient this)]
        (persistent! (reduce conj! vc values)))))
  (data-seq [this]
    (seq this)))

;; ## Seq Implementation of CombitData

(extend-type clojure.lang.ISeq 
  CombitData
  (element-count [this]
    (count this))
  (get-at [this index]
    (nth this index))
  (get-elements [this indices]
    (map #(nth this %) indices))
  (set-at [this index value]
    (concat
      (take (dec index) this)
      [value]
      (drop (inc index) this)))
  (set-elements [this indices values]
    (reduce 
      (fn [sq [i v]]
        (set-at (vec sq) i v))
      this
      (map-indexed #(vector %2 (get-at values %1)) indices)))
  (remove-elements [this indices]
    (let [remove-element? (set indices)]
      (keep-indexed #(when-not (remove-element? %) %) this)))
  (concat-elements [this values]
    (if-not (coll? values)
      (u/throw-error "concat-elements", "expected collection to append, given: " values)
      (concat this values)))
  (data-seq [this]
    this))

