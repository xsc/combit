(ns ^ { :doc "Combit Components"
        :author "Yannick Scherer" }
  combit.core
  (:use [combit.utils :as u]
        [combit.data :as data]))

;; ## Combit
;;
;; Combit provides intuitive handling of fixed-size sequences. Its main parts
;; are _components_ that take a series of input sequences and produce a series
;; of output sequences.
;;
;; It aims at providing a concise description of the operations performed on such 
;; sequences.
;;
;; ### Components
;;
;; A Combit component takes a seq of input data and produces a tupel containing the 
;; given inputs, as well as a seq of output data.
;;
;;    (component [a 32] [b 16 c 16] ...)
;;    ; =>
;;    (fn [[a-in & _] & _] 
;;      (vector [a-in] ...))
;;
;; Combit components consist of sets of output manipulation functions, taking the inputs
;; and the current state of the outputs as parameters and producing a tupel of inputs destined
;; for a subsequent processing function, as well as the seq of newly-generated outputs.
;; These transformation functions are applied in the order they are given.

(declare input output)

(defn- normalize-specs
  "Convert a seq of `[symbol block-spec symbol block-spec ...]` to a seq of
   `[[symbol normalized-spec] [symbol-normalized-spec]]` pairs."
  [specs]
  (map
    (fn [[sym spec]]
      (vector
        sym
        (cond (and (integer? spec) (pos? spec)) (-> {} (assoc :width spec))
              (and (map? spec) (contains? spec :width)) spec
              :else (u/throw-error "component" "Unknown Input/Output specification: " spec))))
    (partition 2 specs)))

(defn- normalize-transformations
  "Normalize the transformations in the `component` body."
  [transformations]
  (vec transformations))

(defn- create-component-let-bindings
  [f pairs]
  (apply concat
         (map-indexed
           (fn [index [sym spec]]
             `[~sym (~f ~index ~(:width spec))])
           pairs)))

(defn run-component-transformations
  "Run the given transformations on the given input and output
   sequences."
  [inputs outputs transformations]
  (reduce
    (fn [outputs transform]
      (transform inputs outputs))
    outputs
    transformations))

(defmacro component
  "Create new component."
  [inputs outputs & transformations]
  (let [input-pairs (normalize-specs inputs)
        output-pairs (normalize-specs outputs)
        input-count (count input-pairs)
        output-count (count output-pairs)
        transform (normalize-transformations transformations)]
    `(let [~@(concat
               (create-component-let-bindings `input input-pairs)
               (create-component-let-bindings `output output-pairs))]
       (fn [i# & _#]
         (let [in#  (vec (take ~input-count i#))
               out# ~(vec (map (fn [[_ spec]]
                                 `(u/new-vector ~(:width spec))) 
                               output-pairs))
               out# (run-component-transformations in# out# ~transform)
               out# (vec (take ~output-count out#))]
           out#)))))

;; ### Combinations
;;
;; The `>>` function combines transformations by supplying the output part emitted by a 
;; transformation as input to the subsequent one.
;;
;;    (>> A B C) ;=> (>> (>> A B) C)
;;    (>> A B) ;=> (fn [i o] (B (second (A i o)) o))

(defn >>
  "Combine a number of transformation functions into a new one, passing outputs from one
   transformation as inputs to the subsequent one."
  [& transformations]
  (reduce
    (fn [t1 t2]
      (fn [inputs outputs]
        (let [o (cond (fn? t1) (t1 inputs outputs)
                      (coll? t1) (map (fn [t] (t inputs outputs)) t1)
                      :else (u/throw-error ">>" "Not a valid transformation: " t1))
              o (cond (fn? t2) (t2 o outputs)
                      (coll? t2) (reduce
                                   (fn [outputs [tx ox]]
                                     (tx [ox] outputs))
                                   outputs
                                   (map vector t2 o))
                      :else (u/throw-error ">>" "Not a valid transformation: " t2))]
          o)))
    transformations))

;; ### Input Getter / Output Setter

(declare input-getter output-setter)

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

(defn input
  "Create function that, when supplied with an input selector specification (i.e. the indices
   of elements to extract from a sequence), produces an input getter function."
  [index width]
  (->
    (fn [spec]
      (let [f (input-getter width spec)]
        (fn [inputs outputs]
          (let [input (nth inputs index)]
            [(f input)]))))
    (wrap-with-range width)))

(defn output
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

(defn const
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

(defn- input-getter
  [width elements-to-get]
  (fn [data]
    (map
      (fn [index]
        (data/get-at data index))
      elements-to-get)))

(defn- output-setter
  [width elements-to-set]
  (fn [[data & _] output]
    (reduce
      (fn [output [src-index dest-index]]
        (data/set-at output dest-index (data/get-at data src-index)))
      (vec output)
      (map vector (range) elements-to-set))))

;; ## Utilities

(defn generate-output-binding
  [inputs outputs sym input-form]

  ;; Check
  (when-not (vector? sym)
    (u/throw-error "with-outputs" sym " should be vector!"))
  (doseq [s sym]
    (when-not (symbol? s)
      (u/throw-error "with-outputs" "output vector for " input-form " contains non-symbol " s)))

  ;; Create Binding
  (vector
    sym 
    `(let [o# (~input-form ~inputs ~outputs)]
       (map const o#))))

(defmacro with-outputs
  [output-bindings & body]
  
  ;; Check
  (when-not (vector? output-bindings)
    (u/throw-error "with-outputs" output-bindings " should be a vector!"))
  (when-not (= (rem (count output-bindings) 2) 0)
    (u/throw-error "with-outputs" "Expecting an even number of binding forms."))

  ;; Create function.
  (let [binding-pairs (partition 2 output-bindings)
        input-sym (gensym "i_")
        output-sym (gensym "o_")
        transform (normalize-transformations body)]
    `(fn [~input-sym ~output-sym]
       (let [~@(mapcat
                 (fn [[sym input-form]]
                   (generate-output-binding input-sym output-sym sym input-form))
                 binding-pairs)]
         (run-component-transformations ~input-sym ~output-sym ~transform)))))
