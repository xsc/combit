(ns ^{ :doc "Combit Data"
       :author "Yannick Scherer" }
  combit.data
  (:use [combit.utils :as u]))

;; ## CombitData
;;
;; Combit Data Blocks are vectors, which might be accessed using different strategies.
;; (first element is 0, last element is 0, ...). Vectors offer the best manipulation
;; characteristics which is why they will be used.

(def ^:dynamic *index-transformation* #(identity %2))

(defn get-index
  [c i]
  (*index-transformation* c i))

(defn get-elements
  [data indices]
  (if-not (vector? data)
    (u/throw-error "get-elements" "expected vector as input; given: " data)
    (let [c (count data)]
      (->>
        (map #(get-index c %) indices)
        (map #(get data %))
        vec))))

(defn set-elements
  [data indices values]
  (if-not (vector? data)
    (u/throw-error "set-elements" "expected vector as input, given: " data)
    (let [c (count data)
          vc (transient data)]
      (persistent!
        (reduce
          (fn [vc [i v]]
            (assoc! vc i v))
          vc
          (map vector
               (map #(get-index c %) indices)
               values))))))

(defn remove-elements
  [data indices]
  (if-not (vector? data)
    (u/throw-error "set-elements" "expected vector as input, given: " data)
    (let [c (count data)
          remove-element? (set (map #(get-index c %) indices))
          vc (transient [])]
      (persistent!
        (reduce
          (fn [vc [i e]]
            (if (remove-element? i)
              vc
              (conj! vc e)))
          vc
          (map vector (range) data))))))

(defn concat-elements
  [& data]
  (let [vc (transient [])]
    (persistent!
      (reduce
        (fn [vc block]
          (if-not (vector? data)
            (u/throw-error "concat-elements", "expected vectors as input, given: " data)
            (let [c (count block)]
              (reduce (fn [vc i]
                        (conj! vc (get block i)))
                      vc
                      (map #(get-index c %) (range c))))))
        vc
        data))))

(defn set-at
  "Set the element at the given position to the given value."
  [data index value]
  (if-not (vector? data)
    (u/throw-error "set-at", "expected vector as input, given: " data)
    (assoc data index value)))

(defn get-at
  "Get the element at the given position."
  [data index]
  (if-not (vector? data)
    (u/throw-error "get-at", "expected vector as input, given: " data)
    (get data index)))

(defn remove-at
  "Remove the element at the given position."
  [data index]
  (remove-elements (vec data) [index]))

(defn take-elements
  "Create new data consisting of the elements with indices 0 to n-1."
  [n data]
  (get-elements (vec data) (range n)))

(defn drop-elements
  "Create new data consisting of the elements with indices n to N."
  [n data]
  (remove-elements (vec data) (range n)))
