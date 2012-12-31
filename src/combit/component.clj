(ns ^{ :doc "Combit Component Creation Helpers"
       :author "Yannick Scherer" }
  combit.component
  (:use [combit.utils :as u]
        [combit.combine :as comb :only [combine]]
        [combit.input-output :as io :only [input-data output-data const-data]]))

;; ## Helpers

(defn normalize-specs
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

(defn normalize-transformations
  "Normalize the transformations in the `component` body."
  [transformations]
  (vec transformations))

(defn create-component-let-bindings
  "Create let-bindings for `component` macro."
  [f pairs]
  (apply concat
         (map-indexed
           (fn [index [sym spec]]
             `[~sym (~f ~index ~(:width spec))])
           pairs)))

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
  [inputs outputs transformations]
  (reduce
    (fn [outputs transform]
      (if transform
        (if-let [new-outputs (transform inputs outputs)]
          new-outputs
          outputs)
        outputs))
    outputs
    transformations))

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
  "Wrap a component so it converts (c [a b]) to (>> [a b] c) if there are any
   functions inside the given input seq."
  [f]
  (fn x
    ([] x) 
    ([inputs _] (x inputs))
    ([inputs] 
     (if (some fn? inputs)
       (comb/combine (map #(if (fn? %) % (io/const-data %)) inputs) f)
       (f inputs nil)))))

(defmacro new-component
  "Create new component."
  [inputs outputs & transformations]
  (let [input-pairs (normalize-specs inputs)
        output-pairs (normalize-specs outputs)
        input-count (count input-pairs)
        output-count (count output-pairs)
        transform (normalize-transformations transformations)]
    `(let [~@(concat
               (create-component-let-bindings `io/input-data input-pairs)
               (create-component-let-bindings `io/output-data output-pairs))
           initial-outputs# ~(create-component-initial-outputs output-pairs)]
       (wrap-component
         (fn [inputs# ~'_]
           (let [inputs# (seq inputs#)]
             (check-component-seq inputs# ~input-count)
             (let [outputs# (seq
                              (run-component-transformations 
                                inputs# initial-outputs# ~transform))]
               (check-component-seq outputs# ~output-count)
               (vec outputs#))))))))
