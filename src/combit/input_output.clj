(ns ^{ :doc "Input Getters, Output Setters, Constants, ..."
       :author "Yannick Scherer" }
  combit.input-output
  (:use [combit.data :as data]
        [combit.utils :as u]))

;; ## Helpers

(defn- range-n
  [from to]
  (cond (= from to) from
        (> to from) (range from (inc to))
        :else (reverse (range to (inc from)))))

;; ## Inputs

(defn input-data
  "Create function that, when supplied with an input selector specification (i.e. the indices
   of elements to extract from a sequence), produces the associated data."
  [data width]
  (let [data (vec (data/take-elements width data))]
    (fn x 
      ([] data)
      ([spec]
       (cond (and (integer? spec) (>= spec 0) (< spec width))
             (vector (data/get-at data spec))
             (and (coll? spec) (every? #(and (>= % 0) (< % width)) spec))
             (data/get-elements data spec)
             :else (u/throw-error "input-data"
                                  "not a valid data range (width: " width "): " spec)))
      ([lower upper]
       (if (and (< lower width) (< upper width))
         (x (range-n lower upper))
         (u/throw-error "input-data"
                        "not a valid data range (width: " width "): " lower " -> " upper))))))

;; ## Outputs

(defn output-data
  [index width]
  (fn x
    ([] (fn [outputs [data & _]]
          (let [data (if (> (data/element-count data) width)
                       (data/take-elements width data)
                       data)]
            (assoc outputs index data))))
    ([spec]
     (fn [outputs [data & _]]
       (assoc outputs index
              (let [output (get outputs index)]
                (cond (and (integer? spec) (>= spec 0) (< spec width))
                      (->> 
                        (data/get-at data 0)
                        (data/set-at output spec))
                      (and (coll? spec) (every? #(and (>= % 0) (< % width)) spec))
                      (data/set-elements output spec data)
                      :else (u/throw-error "output-data"
                                           "not a valid data range (width: " width "): " spec))))))
    ([lower upper]
     (if (and (< lower width) (< upper width))
       (x (range-n lower upper))
       (u/throw-error "output-data"
                      "not a valid data range (width: " width "): " lower " -> " upper)))))

(defn const-data
  "Create function that, when supplied with an input selector specification (i.e. the indices
   of elements to extract from a sequence), produces the associated constant data."
  [data]
  (input-data data (count data)))
