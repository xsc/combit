(ns ^{ :doc "Input Getters, Output Setters, Constants, ..."
       :author "Yannick Scherer" }
  combit.input-output
  (:use [combit.data :as data :only [get-elements set-elements]]
        [combit.utils :as u]))

(defn- input-getter
  "Create function that, when supplied with CombitData returns the elements given by index."
  [width elements-to-get]
  (if (some #(or (>= % width) (< % 0)) elements-to-get)
    (u/throw-error "input-getter" "not a valid data range (width is " width "): " elements-to-get))
    (fn [data]
      (data/get-elements data elements-to-get)))

(defn- output-setter
  "Create function that, when supplied with CombitData as input and output, alters the output
   at the given indices."
  [width elements-to-set]
  (if (some #(or (>= % width) (< % 0)) elements-to-set)
    (u/throw-error "input-getter" "not a valid data range (width is " width "): " elements-to-set)
    (fn [[data & _] output]
      (data/set-elements output elements-to-set data))))

(defn- wrap-with-range
  "Wrap a function that expects a single parameter (a range specification) with the possibility to
   not specify a range at all (= \"use full range\") or to specify it as lower and upper bounds."
  [f width]
  (fn 
    ([] (f (range width)))
    ([spec] (if (coll? spec)
              (f spec)
              (f (vector spec))))
    ([lower upper]
     (if (and (integer? lower) (integer? upper))
       (f
         (if (>= upper lower)
           (range lower (inc upper))
           (reverse (range upper (inc lower)))))
       (u/throw-error "input/output" "Invalid lower/upper bound: " lower " -> " upper)))))

(defn input-data
  "Create function that, when supplied with an input selector specification (i.e. the indices
   of elements to extract from a sequence), produces an input getter function."
  [index width]
  (->
    (fn [spec]
      (let [f (input-getter width spec)]
        (fn [inputs & _]
          (let [input (nth inputs index)]
            [(f input)]))))
    (wrap-with-range width)))

(defn output-data
  "Create function that, when supplied with an output selector specification (i.e. the indices
   of elements to replace in a sequence), produces an output setter function."
  [index width]
  (-> 
    (fn [spec]
      (let [f (output-setter width spec)]
        (fn [inputs outputs]
          (let [output (nth outputs index)]
            (concat
              (take index outputs)
              [(f inputs output)]
              (drop (inc index) outputs))))))
    (wrap-with-range width)))

(defn const-data
  "Create function that, when supplied with an input selector specification (i.e. the indices
   of elements to extract from a sequence), produces an input getter function operating on the
   constant data supplied."
  [data]
  (let  [width (data/element-count data)]
    (->
      (fn [spec]
        (let [f (input-getter width spec)]
          (fn [_ _]
            [(f data)])))
      (wrap-with-range width))))
