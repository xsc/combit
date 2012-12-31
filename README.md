# combit

[![Build Status](https://travis-ci.org/xsc/combit.png?branch=master)](https://travis-ci.org/xsc/combit)

Combit tries to offer concise and intuitive ways to manipulate fixed-size containers.

## Usage

__Leiningen:__

```clojure
[combit "0.1.0"]
```

## Components

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
  (`\[in 32\] \[out 32\]`); 
- the body of the component consists of a series of "transformation functions" that will 
  sequentially be executed on the component inputs and the current outputs;
- and finally, the `>>` function combines two transformation functions into a new one, connecting
  the outputs of the first one to the inputs of the subsequent one.

## License

Copyright 2012 Yannick Scherer

Distributed under the Eclipse Public License, the same as Clojure.
