(ns ^ { :doc "Combit Components"
        :author "Yannick Scherer" }
  combit.core
  (:use [combit.utils :as u]
        [combit.data :as data]
        [combit.component :as c]
        [combit.input-output :as io :only [const-data connect->> write->>]]))

;; ## Combit
;;
;; Combit provides intuitive handling of fixed-size sequences. Its main parts
;; are _components_ that take a series of input sequences and produce a series
;; of output sequences.
;;
;; It aims at providing a concise description of the operations performed on such 
;; sequences.

;; ### Combinator

(def >> 
  "Combit Combination Operator. (see `combit.input-output/transform->>`)"
  io/connect->>)

(def >>*
  "Write a value directly to an output, using the `>>` operator."
  io/write->>)

;; ### Components

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

;; ### Utilities

(defn const
  "Create constant that is accessible using selectors (like the input functions)."
  [v]
  (io/const-data v))

(defn value
  "If you want to pass a constant value to the input of a component or output
   setter using `>>`, you have to wrap it using this function."
  [v]
  (vector v))

(defmacro with-outputs
  "Variant of `let` designed for components. The body may consist of a series
   of transform functions."
  [output-bindings & transformations]
  `(let [~@(mapcat (fn [[sym input-form]]
                     `[~sym (let [i# ~input-form]
                              (map const i#))]) 
                   (partition 2 output-bindings))
         transform# ~(vec transformations)]
     (fn [o#]
       (c/run-component-transformations o# transform#))))

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

(defmacro primitive
  "Create new primitive (a component with exactly one one-element output where every input 
   consists of exactly one element) using the given forms as a function body."
  [inputs & body]
  (let [input-map (map vector inputs (repeatedly (partial gensym "input_")))]
    `(gate ~(vec (map second input-map)) [o#]
       (let [~@(mapcat (fn [[input sym]]
                         `[~input (~sym)]) input-map)
             ~@(mapcat (fn [[input sym]]
                         `[~input (get-at ~input 0)]) input-map)
             v# (do ~@body)]
           (>>* [v#] (o#))))))

(defmacro def-primitive
  "Define new primitive."
  [id inputs & body]
  `(def ~id (primitive-fn ~inputs ~@body)))
