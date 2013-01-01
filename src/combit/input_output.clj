(ns ^{ :doc "Input Getters, Output Setters, Constants, ..."
       :author "Yannick Scherer" }
  combit.input-output
  (:use [combit.data :as data :only [get-elements set-elements]]
        [combit.utils :as u]))

;; ## Input/Output Selection

(defn- get-input
  "Get elements from input data."
  [data width elements-to-get]
  (if (some #(or (>= % width) (< % 0)) elements-to-get)
    (u/throw-error "input-getter" "not a valid data range (width is " width "): " elements-to-get)
    (data/get-elements data elements-to-get)))

(defn- set-output
 "Set elements in output data."
  [width elements-to-set data output]
  (if (some #(or (>= % width) (< % 0)) elements-to-set)
    (u/throw-error "input-getter" "not a valid data range (width is " width "): " elements-to-set)
    (data/set-elements output elements-to-set data)))

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
   of elements to extract from a sequence), produces the associated data."
  [data width]
  (->
    (partial get-input data width)
    (wrap-with-range width)))

(defn output-data
  "Create function that, when supplied with an output selector specification (i.e. the indices
   of elements to replace in a sequence), produces an output setter function."
  [index width]
  (-> 
    (fn [spec]
      (fn [inputs outputs]
        (let [output (nth outputs index)]
          (concat
            (take index outputs)
            [(set-output width spec (first inputs) output)]
            (drop (inc index) outputs)))))
    (wrap-with-range width)))

(defn const-data
  "Create function that, when supplied with an input selector specification (i.e. the indices
   of elements to extract from a sequence), produces the associated constant data."
  [data]
  (input-data data (data/element-count data)))

;; ## Write to Outputs

(defn write-inputs
  "Create function that connects the given inputs to the given output setter function(s).
   Output setters can be given as:
   - a single function: write to the given output
   - a set of functions: write the same value to all given outputs
   - a seq of functions: write the "
  [inputs output-setters]
  (if-not inputs identity
    (letfn [(run-setters [setters o]
              (reduce 
                (fn [o s]
                  (s inputs o))
                o
                setters))]
    (cond 
      (set? output-setters) (partial run-setters (seq output-setters))
      (fn? output-setters) (partial run-setters (vector output-setters))
      (coll? output-setters) (->>
                               (map (fn [input output]
                                      (write-inputs [input] output))
                                    inputs
                                    output-setters)
                               (apply comp))
      :else (u/throw-error "write-inputs" "invald output setter specification: " output-setters)))))

;; The `connect->>` function is aimed at providing a concise specification format for
;; the sequential connection of components. Example:
;;
;;     (connect->> [(i 0 7)]
;;       (swapper)   ;; Components return themselves
;;       (splitter)
;;       [(a) (b)])
;;

(defn connect->>
  "Create function that transforms a given seq of output blocks. Of the given n 
   forms, the first n-1 ones will be evaulated by wrapping them in `(->> ...)`
   whilst the n-th form has to be an actual output setter function (or a seq of those), 
   getting the final data to write, as well as the current output seq, and producing the 
   new output state."
  [initial-inputs & transformations]
  (if-not (seq transformations)
    identity
    (let [output-setters (last transformations)
          transformers (reverse (drop 1 (reverse transformations)))]
      (write-inputs
        (reduce
          (fn [next-inputs t]
            (apply t next-inputs))
          initial-inputs
          transformers)
        output-setters))))

;; The above example could also be rewritten (since the first parameter is a single-element
;; vector) to:
;;
;;     (write->> (i 0 7)
;;       (swapper)
;;       (splitter)
;;       [(a) (b)])
;;

(defn write->>
  "Create function that writes the given data to the given output setter function."
  [v & transformations]
  (apply connect->> (vector v) transformations))
