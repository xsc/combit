(ns ^{ :doc "Combit Component Creation Helpers"
       :author "Yannick Scherer" }
  combit.component
  (:use [combit.utils :as u]
        [combit.input-output :as io :only [input-data output-data const-data]]))

;; ## Helpers

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

;; ### Components
;;
;; Combit components consist of sets of output manipulation functions, taking the inputs
;; and the current state of the outputs as parameters and producing a new seq of outputs
;; to either be used by some subsequent processing function or the output generation.
;; These transformation functions are applied in the order they are given.

(defn wrap-component
  "Make a component function return itself, when called without arguments."
  [f]
  (fn x
    ([] x)
    ([& args] (apply f args))))

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
       (wrap-component
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
             outputs#))))))
