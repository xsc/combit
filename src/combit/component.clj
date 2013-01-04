(ns ^{ :doc "Combit Component Creation Helpers"
       :author "Yannick Scherer" }
  combit.component
  (:use [combit.utils :as u]
        [combit.combination :as combine :only [sequential*]]
        [combit.input-output :as io :only [input-data output-data const-data]]))

;; ## Output Transformation

(defn- transform-outputs
  "Based on the value(s)/value function to write, a transformer function f and the current 
   state of the outputs, create new outputs."
  [current-outputs data f]
  (let [current-outputs (vec current-outputs)
        data (if (fn? data) (data) data)]
    (cond (not (coll? data))
          (u/throw-error "transform-outputs" "data to be written is no collection: " data)
          (set? f) (reduce 
                     (fn [o f]
                       (f o data))
                     current-outputs
                     f)
          (coll? f) (reduce 
                      (fn [o [f b]]
                        (f o [b]))
                      current-outputs 
                      (map vector f data))
          :else (f current-outputs data))))

(defn transform-outputs->>
  "Based on a series of value creation functions and a writer function, transform the given
   outputs. Example:

     (transform-outputs->> [[:out1 :out2] [nil nil]] 
       [[:out3]]
       ;; -> will be passed as-is to the next \"component\"
       (juxt first first)
       ;; -> non-function input will be converted to [[:out3 :out3]]
       #(vector (first %1) %2)
       ;; -> result will be [[:out1 :out2] [:out3 :out3]]
       )
  "
  [current-outputs & args]
  (when (< (count args) 2)
    (u/throw-error "transform-outputs->>" 
                   "expected at least one value/value function and the output setter."))
  (let [c (count args)
        setter (last args)
        initial-value (first args)
        components (take (- c 2) (rest args))
        value-transformer (combine/sequential* components)]
    (transform-outputs
      (vec current-outputs)
      (apply value-transformer initial-value)
      setter)))

;; ## Components

;; ### Component Wrapper 

(defn wrap-component
  "Wrap component function to exhibit the expected component behaviour."
  [c input-count]
  (-> c
    (u/curry input-count)))

;; ### Component Specification

(defn normalize-io-spec
  [spec]
  (cond (and (integer? spec) (pos? spec)) (-> {} (assoc :width spec))
        (and (map? spec) (contains? spec :width)) spec
        :else (u/throw-error "component" "Unknown Input/Output specification: " spec)))

(defn normalize-specs
  "Convert a seq of `[symbol block-spec symbol block-spec ...]` or a map of 
   `{ :spec ... :count ... }` to a map with the keys `:as`
    (if supplied) and `:specs` of the form`[[symbol normalized-spec] 
    [symbol normalized-spec]]` pairs."
  [specs]
  (if (map? specs)
    (let [{ c :count :keys [as spec]} specs]
      (when-not (and spec c as)
        (u/throw-error "component" 
                       "not a valid input/output specification (:spec, :count or :as) missing: "
                       specs))
      (when-not (and (integer? c) (pos? c))
        (u/throw-error "component" ":count has to be a positive integer in: " specs))
      (-> {}
        (assoc :as as)
        (assoc :specs
          (let [s (normalize-io-spec spec)]
            (for [n (range c)] `[~(gensym) ~s])))))
    (let [pairs (partition 2 specs)]
      (-> {}
        (assoc :as nil)
        (assoc :specs
               (map (fn [[sym spec]] 
                      [sym (normalize-io-spec spec)])
                    pairs))))))

;; ### Generation Helpers

(defn normalize-transformations
  "Normalize the transformations in the `component` body."
  [transformations]
  (mapcat
    (fn [x]
      (if (coll? x)
        (normalize-transformations x)
        (vector x)))
     transformations))

(defn create-component-initial-outputs
  "Create intial output vector for `component` macro,"
  [pairs]
  (vec
    (map 
      (fn [[sym spec]]
        (cond (contains? spec :initial) (:initial spec)
              (contains? spec :width) `(u/new-vector ~(:width spec))
              :else (u/throw-error "component" "malformed output spec: " spec)))
      pairs)))

;; ### Run Helpers

(defn run-component-transformations
  "Run the given transformations on the given input and output
   sequences."
  [outputs transformations]
  (reduce
    (fn [outputs transform]
      (if transform
        (if-let [new-outputs (transform outputs)]
          new-outputs
          outputs)
        outputs))
    outputs
    (normalize-transformations transformations)))

(defn check-component-seq
  "Check if the given parameter is a seq with the given element count."
  [s expected-count]
  (let [c (count s)]
    (when-not (= c expected-count)
      (u/throw-error "check-component-seq" "expected " expected-count " data block(s), got " c))))

;; ### Generation Macro
;;
;; Combit components consist of sets of output manipulation functions, taking the inputs
;; and the current state of the outputs as parameters and producing a new seq of outputs
;; to either be used by some subsequent processing function or the output generation.
;; These transformation functions are applied in the order they are given.

(defmacro new-component
  "Create new component."
  [inputs outputs & transformations]
  (let [{ input-pairs :specs input-as :as } (normalize-specs inputs)
        { output-pairs :specs output-as :as } (normalize-specs outputs)
        input-symbols (map (fn [[sym spec]]
                             [(gensym "in_") sym spec]) input-pairs)
        input-count (count input-pairs)
        output-count (count output-pairs)]
    (when-not (pos? input-count)
      (u/throw-error "component" "a component needs at least one input. (use constant data instead)"))
    `(let [initial-outputs# ~(create-component-initial-outputs output-pairs)]
       (->
         (fn [& [~@(map first input-symbols) :as inputs#]]
           (check-component-seq (seq inputs#) ~input-count)
           (let [~@(mapcat 
                     (fn [[gsym sym spec]]
                       `[~sym (io/input-data ~gsym ~(:width spec))])
                     input-symbols)
                 ~@(mapcat 
                     (fn [i [sym spec]]
                       `[~sym (io/output-data ~i ~(:width spec))])
                     (range)
                     output-pairs)
                 ~(or output-as '_) ~(vec (map first output-pairs))
                 ~(or input-as '_) inputs#
                 outputs# (run-component-transformations
                            initial-outputs#
                            ~(vec transformations))]
             (check-component-seq (seq outputs#) ~output-count)
             outputs#))
         (wrap-component ~input-count)))))
