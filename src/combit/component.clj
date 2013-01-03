(ns ^{ :doc "Combit Component Creation Helpers"
       :author "Yannick Scherer" }
  combit.component
  (:use [combit.utils :as u]
        [combit.input-output :as io :only [input-data output-data const-data]]))

;; ## Concept
;;
;; A component is a function that takes a fixed number of inputs and produces another 
;; __function__ without parameters that, when called, returns the final result, consisting
;; of a seq with a fixed number of outputs.
;; 
;; If the component is called with less parameters than it has inputs, it will return
;; an identical component where the given inputs are already set to the given values
;; (see "Currying").
;;
;; This enables concatenation in a uniform way:
;;
;;     (conc a b)
;;     ;; => (comp b a)
;;     
;;     (conc a b c)
;;     ;; => (conc (conc a b) c)

;; ### Currying

(defn curry
  "Create function that enables currying of a given function f with n parameters,
   always passing the given arguments first. If all parameters are given, the function
   will be evaluated."
  ([f n] (curry f n []))
  ([f n args] (fn x 
                ([] x)
                ([& new-args]
                 (let [c (count new-args)]
                   (if (< c n)
                     (curry f (- n c) (concat args new-args))
                     (->>
                       (concat args new-args)
                       (apply f))))))))

(defn curry-constantly
  "Create function that enables currying for a given function f, forcing the final result
   to be `(constantly (vector (f ...)))`. This is useful for concatenation (see `conc`)
   where exactly such results are expected. The result of f will thus be interpreted as
   the value of a single component output."
  [f n]
  (curry (comp constantly vector f) n))

;; ### Concatenation

(defn conc
  "Concatenate components, creating a new one. Since components produce functions without
   parameters, such intermediate values will be evaluated when encountered. "
  [& fs]
  (if (= (count fs) 1)
    (let [r (first fs)]
      (if (fn? r) r (constantly (vector r))))
    (fn [& args]
      (if-let [v (reduce 
                   (fn [r f]
                     (let [next-args (if (fn? r) (r) [r])]
                       (cond (not (fn? f))           f ;; the given value is used as a direct input
                             (not next-args)         f ;; no arguments -> f must produce them
                             (not (coll? next-args))
                             (u/throw-error "conc" "expected output collection, but got: " next-args)
                             :else (apply f next-args))))
                   (constantly args)
                   (concat fs [vector]))]
        v
        (u/throw-error "conc" "last transformer function has not returned a value.")))))


;; ### Output Transformation

(defn- transform-outputs
  "Based on the value(s)/value function to write, a transformer function f and the current 
   state of the outputs, create new outputs."
  [current-outputs v f]
  (let [data (if (fn? v) (v) [v])]
    (when-not (coll? data)
      (u/throw-error "transform-outputs" "data to be written is no collection: " data))
    (cond (coll? f) (reduce 
                      (fn [o [f b]]
                        (f o [b]))
                      current-outputs 
                      (map vector f data))
          (set? f) (reduce 
                     (fn [o f]
                       (f o data))
                     current-outputs
                     f)
          :else (f current-outputs data))))

(defn transform-outputs->>
  "Based on a series of value creation functions and a writer function, transform the given
   outputs. Example:

     (transform-outputs->> [[:out1 :out2] [nil nil]] 
       [:out3]                        
       ;; -> non-function input will be converted to [[:out3]]
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
        fs (take (dec c) args)]
    (transform-outputs current-outputs (apply conc fs) setter)))

;; ## Components

;; ### Component Wrapper 

(defn wrap-component
  "Wrap component function to exhibit the expected component behaviour."
  [c input-count]
  (-> c
    (curry input-count)))

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
        x
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
  (when-not (or (nil? s) (seq? s))
    (u/throw-error "check-component-seq" "expected sequence of data blocks, got: " s))
  (let [c (count s)]
    (when-not (= c expected-count)
      (u/throw-error "check-component-seq" "expected " expected-count " data blocks, got " c))))

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
             (constantly outputs#)))
         (wrap-component ~input-count)))))
