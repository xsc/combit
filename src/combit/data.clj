(ns ^{ :doc "Combit Data"
       :author "Yannick Scherer" }
  combit.data)

;; ## CombitData Protocol

(defprotocol CombitData
  "Protocol for all data processable by Combit."
  (set-at         [this index value] "Set the element at the given position to the given value.")
  (get-at         [this index] "Get the element at the given position.")
  (element-count  [this] "Get the number of elements."))

;; ## Built-In Implementations

(extend-protocol CombitData
  
  clojure.lang.IPersistentVector
  (set-at [this index value]
    (assoc this index value))
  (get-at [this index]
    (get this index))
  (element-count [this]
    (count this))

  clojure.lang.ISeq
  (set-at [this index value]
    (concat
      (take (dec index) this)
      [value]
      (drop (inc index) this)))
  (get-at [this index]
    (nth this index))
  (element-count [this]
    (count this)))

