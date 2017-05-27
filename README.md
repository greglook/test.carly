test.carly
==========

This library provides a generative test framework for verifying the behavior of
stateful systems under a sequence of random operations. You specify the types of
operations and model the expected system behavior, and `test.carly` explores the
input space to try to discover violations.

This project is inspired by Aphyr's excellent
[Jepsen](https://aphyr.com/tags/Jepsen) project as well as Eric Normand's 2017
Clojure/West Talk, [Testing Stateful and Concurrent Systems Using
test.check](https://www.youtube.com/watch?v=r5i_OiZw6Sw).


## Installation

Library releases are published on Clojars. To use the latest version with
Leiningen, add the following dependency to your project definition:

[![Clojars Project](https://clojars.org/mvxcvi/test.carly/latest-version.svg)](https://clojars.org/mvxcvi/test.carly)


## Usage

At a high level, there are three kinds of things to understand:
- The _system_ under test - often this is a stateful component, but may even be
  larger systems abstracted behind a client. It must be cheap to construct and
  tear down systems for testing iterations.
- The _model_ of the system. This should be a simplified, immutable
  representation of the system that can be manipulated locally. For example, a
  data store might be represented by a map holding the stored data.
- Sequences of _operations_ that represent interactions with the system. Each
  operation is applied to the system to produce a result, and the operation may
  check the result against the current model to validate it. Finally, the
  operation may update the model to reflect stateful changes to the system.

In order to use `test.carly` to test a system, you must first define some of
your operations. Lets say that we're testing a simple data store that needs to
support reads, writes, and deletes. Our system state will be a map in an atom,
and the model is a simple map reflecting the same data.

The `defop` macro handles a lot of the boilerplate for you:

```clojure
(require '[test.carly.core :as carly :refer [defop]])

(defop GetEntry
  [k]

  (gen-args
    [context]
    [(gen/elements (:keys context))])

  (apply-op
    [this system]
    (get @system k))

  (check
    [this model result]
    (is (= (get model k) result))))
```

The `GetEntry` operation represents a client looking up a key in the store. It
should return the corresponding value, and the lookup does not impact the model
state.

```clojure
(defop PutEntry
  [k v]

  (gen-args
    [context]
    {:k (gen/elements (:keys context))
     :v gen/large-integer})

  (apply-op
    [this system]
    (swap! system assoc k v)
    v)

  (check
    [this model result]
    (is (= v result)))

  (update-model
    [this model]
    (assoc model k v)))
```

`PutEntry` on the other hand is side-effecting, and associates a new value with
the specified key in both the store and the model.

In these examples the operation arguments are generated from a common _test
context_, which has its own generator. This is pulled out so that there is a
reasonable likelihood of operations overlapping; if each operation generated a
new random key to read or write to the store, the odds of two operations
choosing the same key would be very small. For our example store, we can
generate a context which provides a fixed set of keys to choose from:

```clojure
(def gen-context
  (gen/hash-map :keys (gen/set (gen/fmap (comp keyword str) gen/char-alpha)
                               {:min-elements 1})))
```

The context is passed to a function which should return a vector of generators
for the operations under test. We can construct one easily using the generator
constructors produced by `defop`:

```clojure
(def op-generators
  (juxt gen->ListKeys
        gen->GetEntry
        gen->PutEntry
        gen->RemoveEntry))
```

Finally, we can define a linear test harness to exercise the store:

```clojure
(deftest store-test
  (carly/check-system
    "basic store tests"
    #(atom (sorted-map))
    op-generators
    :context gen-context
    :iterations 20))
```

Of course, the real power is testing concurrently; to do so, use the related
`check-system-concurrent` test function:

```clojure
(deftest ^:concurrent concurrent-test
  (carly/check-system-concurrent
    "concurrent store tests"
    #(atom (sorted-map))
    op-generators
    :context gen-context
    :max-concurrency 4
    :repetitions 5
    :iterations 20))
```

For a full example, see the [example tests](test/test/carly/example_test.clj).


## License

This is free and unencumbered software released into the public domain.
See the UNLICENSE file for more information.
