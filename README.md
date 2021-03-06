# combit

[![Build Status](https://travis-ci.org/xsc/combit.png?branch=master)](https://travis-ci.org/xsc/combit)

Combit tries to offer concise and intuitive ways to manipulate fixed-size containers.

## Usage

__Leiningen:__

```clojure
[combit "0.3.0"]
```

__Examples can be found [here](https://github.com/xsc/combit/tree/master/src/combit/examples).__

## Components

### Basics

A Combit Component is a function that operates on blocks/containers of data, producing again such 
blocks. The basic `component` macro creates such a function:

```clojure
(def swapper
  "Swap the two halves of a 32-element container."
  (component [in 32] [out 32]
    (>>* (in 0 15) (out 16 31))
    (>>* (in 16 31) (out 0 15))))

(swapper (range 32))
;; => ([16 17 18 ... 30 31 0 1 2 ... 13 14 15])
```

__Note:__
- components take a seq of input blocks and produce a seq of output blocks;
- the width (number of elements) of those blocks has to be specified when the component is created
(`[in 32] [out 32]`); 
- the body of the component consists of a series of "transformation functions" that will 
sequentially be executed to transform the initially empty outputs;
- the input and output blocks (`in` and `out`) are functions that, when given either a
single parameter (a single index in the block), a vector of integers (indices in the
block), two parameters (a range which may also be inverted) or no parameters at all (all
available indices) will return the data at the given positions.
- and finally, the `>>*` function writes a single value to a given output (potentially using
intermediate components).

Since a component is a transformation function itself, it can be used with `>>*`:

```clojure
(def reverser
  (component [in 4] [out 4]
    (>>* (in 3 0) (out))))

(def swapper
  (component [in 4] [out 4]
    (>>* (in 0 1) (out 2 3))
    (>>* (in 2 3) (out 0 1))))

(def reverse-swapper
  (component [in 4] [out 4]
    (>>* (in) 
         (reverser) ;; A component actually just returns itself when called without
         (swapper)  ;;   any parameters; this is just for syntax consistency.
         (out))))

(reverse-swapper [1 2 3 4])
;; => ([2 1 4 3])
```

You can have multiple inputs and multiple outputs:

```clojure
(def swapper2
  (component [a 2 b 2] [x 2 y 2]
    (>>* (a) (y))
    (>>* (b) (x))))

(swapper2 [1 2] [3 4])
;; => ([3 4] [1 2])
```

Note that there is the macro `(def-component id [in ...] [out ...] ...)` which can (suprise!) be 
used to create a var containing the desired component. At the moment, it can not be used to associate 
metadata with the created var.

### The `>>*` and `>>` functions

The only difference between `>>*` and `>>` is that the former requires a single value (not a seq of 
values!) as its first parameter, which is then passed to the single input of any subsequent 
component/output. This means, that the following will produce unexpected results:

```clojure
(>>* (swapper2 (input1) (input2))  ;; Wrong!
  (some-component-with-two-inputs) 
  (out))
```

The evaluation of the first function call will produce the seq `((input2) (input1))` which will be
passed completely to `some-component-with-two-inputs`. In this case, an exception will be thrown
(wrong arity) but in others this may go unnoticed. I just couldn't find a way to flexibly distinguish
between simple values and component outputs ...

In short, the call

```clojure
(>>* a ... o)
```

translates to

```clojure
(>> (vector a) ... o)
```

where `>>` expects a seq of values as first parameter, passing it to the inputs of the subsequent function
or output.

### Calling other Components

So far, the examples only demonstrated how to connect _all_ the outputs of a component to _all_ the 
inputs of another one. However, sometimes it is necessary to be able to use outputs of multiple
components, e.g. when performing complex bit-level operations:

```clojure
(def xor
  (component [a 2 b 2] [out 2] 
    ...))

(def-component split-halves 
  [in 4] [upper 2 lower 2]
  (>>* (in 0 1) (upper))
  (>>* (in 2 3) (lower)))

(def-component xor-opposite-halves
  [a 4 b 4] [out 4]
  (with-outputs [[ua la] (split-halves (a))
                 [ub lb] (split-halves (b))]
    (>> (xor (ua) (lb)) (out 0 1))   ;; or: (>> [(ua) (lb)] (xor) (out 0 1))
    (>> (xor (la) (ub)) (out 2 3))))

(xor-opposite-halves [1 0 0 1] [0 0 1 1])
;; => [[0 1 0 1]]
```

`with-outputs` creates new functions to access the outputs of a given transformation. They can again 
be used with the same kinds of index specifications as the component's own input or output blocks. 

Just for comparison, the non-Combit version of this operation, including the split-halves substep:

```clojure
(defn split-halves
  [in]
  (vector
    (vec (take 2 in))
    (vec (take 2 (drop 2 in)))))

(defn xor-opposite-halves
  [[a b]]
  (let [[ua la] (split-halves a)
        [ub lb] (split-halves b)]
    (vector
      (vec
        (concat
          (map bit-xor ua lb)
          (map bit-xor la ub))))))
```

The obvious difference is the liberal use of indices in the `component` version which has a more 
imperative feel to it (as if you were directly manipulating the output data). Also, and I hate to 
say this, the component is really slow (about 20 times slower!) compared to the standard solution. 
But let's just remember this as an entry point for optimization later on: Are function calls really
that expensive (despite the whole "HotSpot can do this!" propaganda) or are the component input/output
checking routines responsible, at least in part?

### Parallelization/Selection

The namespace `combit.combinations` offers means to flexibly combine components in pre-defined ways.
For example, a number of components can be _parallelized_, meaning that the resulting component takes all
the original components' inputs and produces all the original components' outputs:

```clojure
(def-component drop2 [in 4] [out 2]
  (>>* (in 2 3) (out)))

(def-component take2 [in 4] [out 2]
  (>>* (in 0 1) (out)))

(def drop2-take2 (parallel-1 drop2 take2))

(drop2-take2 [1 2 3 4] [5 6 7 8])
;; => ([3 4] [5 6])
```

`parallel-1` can be used with any number of single-input components. There are explicit variants for 
double-input (`parallel-2`) and triple-input (`parallel-3`) ones, as well as the generic form `parallel-n`
whose first parameter is a seq of the different numbers of inputs to be expected.

Additionally, it is possible to select/reorder and drop certain outputs using `select-outputs` or 
`drop-outputs`:

```clojure
(def-component lower-half [in 4] [out 2]
  (>>* (in) 
       (split-halves)
       (select-outputs [1])
       (out)))

(def-component upper-half [in 4] [out 2]
  (>>* (in)
       (split-halves)
       (drop-outputs [1])
       (out)))

(def-component swap-halves [in 4] [a 2 b 2 c 2]
  (>>* (in)
       (split-halves)
       (select-outputs [1 0 1]) ;; you can reorder outputs, as well as duplicate them
       [(a) (b) (c)]))
```

The above `xor-opposite-halves` could be rewritten using parallelization and selection:

```clojure
(def-component xor-opposite-halves [a 4 b 4] [out 4]
  (>> [(a) (b)]          ;; --> [(a) (b)]
      (parallel-1 
        (split-halves) 
        (split-halves))  ;; --> [(a 0 1) (a 2 3) (b 0 1) (b 2 3)]
      (select-outputs
        [0 3 1 2])       ;; --> [(a 0 1) (b 2 3) (a 2 3) (b 0 1)]
      (parallel-2
        (xor)
        (xor))           ;; --> [(xor (a 0 1) (b 2 3)) (xor (a 2 3) (b 0 1))]
      [(out 0 1) (out 2 3)]))
```

### Primitives: the basic building blocks

Combits component syntax can be used to effectively shuffle around bits of containers. However, no
actual processing was implemented so far. So, let's try to write the `xor` component used in the last 
example. First of all, by remembering what a component _is_:

```clojure
(defn xor-block2
  [a b]
  (vector
    (bit-xor (combit.data/get-at a 0)
             (combit.data/get-at b 0))
    (bit-xor (combit.data/get-at a 1)
             (combit.data/get-at b 1))))

(def xor
  (combit.component/wrap-component
    (fn [a b]
      (vector (xor-block2 a b)))))
```

This takes a seq of two inputs and produces a vector of one output. And it works, even being
reasonably fast. Now, the component version, using access to the actual values:

```clojure
(def-component xor [a 2 b 2] [out 2]
  (>>* (xor-block2 (a) (b)) (out)))
```

That's actually pretty nice. But we can get event more concise using primitives. They have 
single-element inputs and exactly one single-element output, making them prime candidates
for some kind of function-like definition format:

```clojure
(def-primitive xor-base [a b]
  (bit-xor a b))
```

This is already a full-fledged component, creating one XOR bit. We could apply it to the relevant
parts:

```clojure
(def-component xor [a 2 b 2] [out 2]
  (>> (xor-base (a 0) (b 0)) (out 0))
  (>> (xor-base (a 1) (b 1)) (out 1)))
```

This actually expresses pretty clearly what's going on, doesn't it?

### Stream Components

Up until now, we had a look on block-based components, processing fixed-size inputs. However, Combit
offers facilities to use those block-based components to create ones that don't have an input limit.
This is achieved using the `combit.stream/wrap-stream-component` or `combit.stream/wrap-stream-gate`
functions which split the given input into small and fitting pieces, then pass them to the underlying
component, finally merging all the results into one seq of outputs:

```clojure
(def even-xor
  ;; uses e.g. one of the above XORs 
  (wrap-stream-component xor [2 2]))
  
(even-xor [1 0 0 1] [1 1 0 0])
;; => ([0 1 0 1])
```

`wrap-stream-gate` just assumes that every input has the size 1. There are shorthands here: 
`def-stream-component` for defining stream components directly, `def-stream-gate` for gates and 
`def-stream-primitive` for primitives. So, finally, the shortest universally usable XOR component 
can be achieved with:

```clojure
(def-stream-primitive xor [a b]
  (bit-xor a b))
```

The next step here is to provide feedback mechanisms where component inputs can be set to the same
component's previous outputs.

## License

Copyright 2012 Yannick Scherer

Distributed under the Eclipse Public License, the same as Clojure. 
