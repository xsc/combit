(ns ^ { :doc "Combit Components"
        :author "Yannick Scherer" }
  combit.core
  (:use combit.data))

;; A Combit component has a number of fixed-size inputs and outputs.
;; The outputs of a component can be connected to the inputs of another one.
;; Data can be accessed by single bits or chunks of bits.

;; ## Get Input/Set Output 

;; ### Helpers

(defn- get-nth-block
  "Ge the nth block from a series of blocks."
  [n blocks]
  (nth blocks n))

(defn- wrap-with-range
  "Wrap the given function (one of `input` or `output`, see below) with the possibility to
   specify two parameters in place of the last one: an upper and lower bound which will be 
   passed as a `range` seq to the given function.
  
   Additionally, if the last parameter if omitted, it will be given as `(range width)`."
  [f]
  (fn
    ([n width] (f n width (range width)))
    ([n width spec] (f n width spec))
    ([n width lower upper]
     (if (and (integer? lower) (integer? upper))
       (->>
         (if (>= upper lower)
           (range lower (inc upper))
           (range upper (dec lower) -1))
         (f n width))))))

;; ### Combit Input Selector
;;
;; A combit input selector is a function that takes a series of n input blocks,
;; and returns a vector of bits.

(defn- new-input-function
  "Create new input function. The specification can be:
   - an integer: get the nth bit.
   - a vector: get the given bits."
  [width spec]
  (cond (integer? spec) (recur width (vector spec))
        (coll? spec) (fn [block]
                       (-> block
                         (set-width width)
                         (get-bits spec)))
        :else (constantly nil)))

(def input
  "Create input selector function. The resulting function will take all available
   input blocks and return the selected bits."
  (wrap-with-range
    (fn [n width spec]
      (let [get-input-bits (new-input-function width spec)
            select-input-block (partial get-nth-block n)]
        (fn [inputs]
          (get-input-bits
            (select-input-block inputs)))))))

;; ### Combit Output Setter
;;
;; A combit output setter is a function that takes a series of n current output blocks 
;; and a block of data, and produces n new output blocks.

(defn- new-output-function
  "Create new output function. The specification can be:
   - an integer: set the nth bit to the given value
   - a vector: set the given bits in order."
  [width spec]
  (cond (integer? spec) (recur width (vector spec))
        (coll? spec) (fn [block new-data-block]
                       (if-let [data (block-data new-data-block)]
                         (reduce
                           (fn [block [n value]]
                             (when (< n width)
                               (set-bit block n value)))
                           (set-width block width)
                           (map vector spec data))
                         block))
        :else (fn [block _] block)))

(def output
  "Create output setter function. The resulting function will take the current state
   of output blocks and return new outputs according to the given data."
  (wrap-with-range
    (fn [n width spec]
      (let [set-output-bits (new-output-function width spec)
            select-output-block (partial get-nth-block n)]
        (fn [outputs data]
          (if-let [output-block (set-output-bits
                                  (select-output-block outputs)
                                  data)]
            (assoc (vec outputs) n output-block)
            outputs))))))

;; ### Connector
;;
;; The `>>` function connects an input selector to an output setter. The resulting
;; function takes a series of input blocks and a series of current output blocks
;; and produces a new set of output blocks.

(defn >>
  [get-input & processors]
  (let [processor-count (count processors)
        output-setters (let [o (last processors)]
                         (if (coll? o) o (vector o)))
        processors (take (dec processor-count) processors)
        process-fn (apply comp identity (reverse processors))]
    (fn [inputs outputs]
      (let [initial-inputs (if (coll? get-input)
                             (map (fn [f] (f inputs)) get-input) 
                             (vector (get-input inputs)))
            final-inputs (process-fn initial-inputs)]
        (reduce
          (fn [outputs [setter input]]
            (setter outputs input))
          outputs
          (map vector output-setters final-inputs))))))

;; ### Component Macro
;;
;; The `component` macro declares a function with a certain number of input and
;; output blocks. These blocks have a fixed width. The body of the macro may contain
;; an unlimited series of functions taking the given input blocks and the current state
;; of output blocks as parameters and producing a fresh set of output blocks.
;;
;; For example, to swap the two halves of a 32-bit block, you could create the 
;; following function:
;;
;;     (def swapper
;;       (component [in 32] [out 32]
;;         (>> (in 0 15) (out 16 31))
;;         (>> (in 16 31) (out 0 15))))

(defmacro component
  [inputs outputs & body]
  (let [input-pairs (partition 2 inputs)
        output-pairs (partition 2 outputs)]
    `(let [~@(apply concat
               (map-indexed (fn [i [sym width]]
                              `[~sym (partial input ~i ~width)])
                            input-pairs))
           ~@(apply concat
               (map-indexed (fn [i [sym width]]
                              `[~sym (partial output ~i ~width)])
                            output-pairs))]
       (let [component-fn# (reduce
                             (fn [inner-fn# f#]
                               (fn [inputs#]
                                 (f# inputs# (inner-fn# inputs#))))
                             (constantly
                               (vector ~@(map (fn [[_ width]] 
                                                `(create-block ~width)) 
                                              output-pairs)))
                             ~(vec body))]
         (fn [inputs#]
           (->> inputs#
             (map data->block)
             (component-fn#)))))))

;; ## Utilities

(defn split>>
  "Create new output setter function that delivers the same input to multiple output
   setters."
  [& fs]
  (if (seq fs)
    (fn [outputs data]
      (reduce
        (fn [outputs f]
          (f outputs data))
        outputs
        fs))
    (throw (Exception. "split>> needs at least one output setter."))))

(defmacro join>>
  "Create component that joins a series of inputs of the given widths into
   a single output."
  [& widths]
  (let [total-width (reduce + widths)
        inputs (map vector (repeatedly gensym) widths)
        output (gensym)]
    `(component [~@(flatten inputs)] [~output ~total-width]
       ~@(->
           (reduce 
             (fn [[pos fns] [input width]]
               (let [next-pos (+ pos width)]
                 (vector next-pos
                         (cons
                           `(>> (~input 0 ~(dec width))
                                (~output ~pos ~(dec next-pos)))
                           fns))))
             [0 []]
             (reverse inputs))
           second))))
