(ns ^ { :doc "Combit Components"
        :author "Yannick Scherer" }
  combit.core
  (:use [combit.utils :as u]
        [combit.data :as data]
        [combit.component :as c]
        [combit.combine :as comb]
        [combit.input-output :as io :only [const-data]]))

;; ## Combit
;;
;; Combit provides intuitive handling of fixed-size sequences. Its main parts
;; are _components_ that take a series of input sequences and produce a series
;; of output sequences.
;;
;; It aims at providing a concise description of the operations performed on such 
;; sequences.

(def >> 
  "Combit Combination Operator. (see `combit.combine/combine`)"
  comb/combine)

(defmacro component
  "Create new component."
  [& args]
  `(c/new-component ~@args))

(defmacro def-component
  "Define new component."
  [id inputs outputs & transformations]
  `(def ~id 
     (component ~inputs ~outputs
       ~@transformations)))

;; ## Utilities

;; ### Utility Helpers

(defn- check-bindings
  "Check let-style bindings for outputs/values."
  [src-fn check-fn? bindings]
  (when-not (vector? bindings)
    (u/throw-error src-fn bindings " should be a vector!"))
  (when-not (= (rem (count bindings) 2) 0)
    (u/throw-error src-fn "Expecting an even number of binding forms."))
  (doseq [[sym input-form] (partition 2 bindings)]
    (when-not (vector? sym)
      (u/throw-error src-fn  sym " should be vector!"))
    (doseq [s sym]
      (when-not (check-fn? s)
        (u/throw-error src-fn "output vector for " input-form " contains non-symbol " s)))))

(defn- generate-transform-function
  [src-fn gen-fn bindings body]
  (let [binding-pairs (partition 2 bindings)
        input-sym (gensym "i_")
        output-sym (gensym "o_")
        transform (c/normalize-transformations body)]
    `(fn [~input-sym ~output-sym]
       (let [~@(mapcat
                 (fn [[sym input-form]]
                   (gen-fn input-sym output-sym sym input-form))
                 binding-pairs)]
         (run-component-transformations ~input-sym ~output-sym ~transform)))))

;; ### Reusable Outputs

(defn- generate-output-binding
  "Generate transformer-based binding for `with-outputs`."
  [inputs outputs sym input-form]
  (vector
    sym 
    `(let [o# (~input-form ~inputs ~outputs)]
       (map io/const-data o#))))

(defmacro with-outputs
  "Bind output transformations to names usable as input selector."
  [output-bindings & body]
  (check-bindings "with-outputs" symbol? output-bindings)
  (generate-transform-function "with-outputs" generate-output-binding
                               output-bindings
                               body))
  
;; ### Access to Values

(defn- generate-value-binding
  "Generate value-based binding for `with-values`."
  [inputs outputs sym input-form]
  `[~sym (~input-form ~inputs ~outputs)])

(defmacro with-values
  "Bind output transformations to names usable as their actual values."
  [value-bindings & body]
  (generate-transform-function 
    "with-values" 
    generate-value-binding
    value-bindings
    body))

(defn const-value
  "Create function that can directly be written to outputs."
  [v]
  ((io/const-data v)))

;; ### Gates
;; 
;; Gates take single-element inputs and produce single-element outputs.

(defmacro gate
  "Create new gate (a component where every input and output consists of exactly
   one element)."
  [inputs outputs & body]
  `(component
     ~(vec (mapcat (fn [sym] `[~sym 1]) inputs))
     ~(vec (mapcat (fn [sym] `[~sym 1]) outputs))
     ~@body))

(defmacro def-gate
  "Define new gate."
  [id inputs outputs & body]
  `(def ~id 
     (gate ~inputs ~outputs ~@body)))

;; ### Primitives
;;
;; These macros are shorthand notations for defining the most basic building blocks of
;; one's infrastructure.

(defmacro primitive-fn
  "Create new primitive (a component with exactly one one-element output where every input 
   consists of exactly one element) using the given forms as a function body."
  [inputs & body]
  (let [input-map (map vector inputs (repeatedly (partial gensym "input_")))]
    `(gate ~(vec (map second input-map)) [o#]
       (with-values [~@(mapcat (fn [[input sym]]
                                  `[[~input] (~sym)]) input-map)]
         (let [~@(mapcat (fn [[input sym]]
                           `[~input (get-at ~input 0)]) input-map)]
           (>> (const-value 
                 (vector
                   (do
                     ~@body))) (o#)))))))

(defmacro def-primitive
  "Define new primitive."
  [id inputs & body]
  `(def ~id (primitive-fn ~inputs ~@body)))
