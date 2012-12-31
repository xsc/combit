# combit

[![Build Status](https://travis-ci.org/xsc/combit.png?branch=master)](https://travis-ci.org/xsc/combit)

Combit tries to offer concise and intuitive ways to manipulate fixed-size containers.

## Usage

__Leiningen:__

```clojure
[combit "0.1.0"]
```

## Components

### Basics

A Combit Component is a function that operates on blocks/containers of data, producing again such 
blocks. The basic `component` macro creates such a function:

```clojure
(def swapper
  "Swap the two halves of a 32-element container."
  (component [in 32] [out 32]
    (>> (in 0 15) (out 16 31))
    (>> (in 16 31) (out 0 15))))

(swapper [(range 32)])
;; => [[16 17 18 ... 30 31 0 1 2 ... 13 14 15]]
```

__Note:__
- components take a seq of input blocks and produce a seq of output blocks;
- the width (number of elements) of those blocks has to be specified when the component is created
(`[in 32] [out 32]`); 
- the body of the component consists of a series of "transformation functions" that will 
sequentially be executed on the component inputs and the current outputs;
- the input and output blocks (`in` and `out`) are functions that, when given either a
single parameter (a single index in the block), a vector of integers (indices in the
block), two parameters (a range which may also be inverted) or no parameters at all (all
available indices) will create a transformation function that accesses the given parts of
inputs or outputs;
- and finally, the `>>` function combines two transformation functions into a new one, connecting
the outputs of the first one to the inputs of the subsequent one.

Since a component is a transformation function itself, it can be used with `>>`:

```clojure
(def reverser
  (component [in 4] [out 4]
    (>> (in 3 0) (out))))

(def swapper
  (component [in 4] [out 4]
    (>> (in 0 1) (out 2 3))
    (>> (in 2 3) (out 0 1))))

(def reverse-swapper
  (component [in 4] [out 4]
    (>> (in) 
        (reverser) ;; A component actually just returns itself, when called without
        (swapper)  ;; any parameters; this is just for syntax consistency.
        (out))))

(reverse-swapper [[1 2 3 4]])
;; => [[2 1 4 3]]

```

You can have multiple inputs and multiple outputs:

```clojure
(def swapper2
  (component [a 2 b 2] [x 2 y 2]
    (>> (a) (y))
    (>> (b) (x))))

(swapper2 [[1 2] [3 4]])
;; => [[3 4] [1 2]]
```

Note that there is the macro `(def-component id [in ...] [out ...] ...)` which can (suprise!) be 
used to create a var containing a desired component. At the moment, it can not be used to associate 
metadata with the created var.

### Complex Combinations 

So far, the examples only demonstrated how to connect _all_ the outputs of a component to _all_ the 
inputs of another one. However, sometimes it is necessary to be able to use outputs of multiple
components, e.g. when performing complex bit-level operations:

```clojure
(def xor
  (component [a 2 b 2] [out 2] 
    ...))

(def xor-opposite-halves
  (let [split-halves (component [in 4] [upper 2 lower 2]
                       (>> (in 0 1) (upper))
                       (>> (in 2 3) (lower)))]
    (component [a 4 b 4] [out 4]
      (with-outputs [[ua la] (>> (a) (split-halves))
                     [ub lb] (>> (b) (split-halves))]
        (>> [(ua) (lb)] (xor) (out 0 1))
        (>> [(la) (ub)] (xor) (out 2 3))))))

(xor-opposite-halves [[1 0 0 1] [0 0 1 1]])
;; => [[0 1 0 1]]
```

`with-outputs` creates new functions to access the outputs of a given transformation. They can again be used with the same kinds of index specifications as the component's own input or output blocks.

## License

Copyright 2012 Yannick Scherer

Distributed under the Eclipse Public License, the same as Clojure.
