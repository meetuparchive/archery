## Archery

### Overview

Archery is a two-dimensional [R-Tree](http://en.wikipedia.org/wiki/R-tree)
written in Scala. The implementation is immutable: adding and removing points
from the tree produces a new tree, leaving the old one untouched. Due to
[structural sharing](http://en.wikipedia.org/wiki/Persistent_data_structure)
this operation is quite efficient.

The name "archery" is a corruption of the word "R-Tree".

### Example Usage

```scala
import archery._

// create some entries
val alice = Entry(Point(9.12F, -4.9F), "alice")
val bob = Entry(Point(2.3F, 4.6F), "bob")
val candice = Entry(Point(4.7F, -1.9F), "candice")
val doug = Entry(Point(5.5F, -3.2F), "doug")

// build a tree with three points
val tree1: RTree[String] = RTree(alice, bob, candice)

// add "doug"
val tree2: RTree[String] = tree1.insert(doug)

// remove "bob"
val tree3: RTree[String] = tree2.remove(bob)

// search from (0,-4) to (10,6), will find "doug"
val bbox: Box = Box(0F, -4F, 10F, 6F)
val results: Vector[Entry[String]] = tree3.search(bbox)

// we can also just ask how many matching entries exist
val n: Int = tree3.count(bbox)
assert(results.length == n)
```

### Contributing

Building this project requires SBT 0.13.0.

After you launch SBT, you can run the following commands:

 * `compile` compile the project
 * `core/test` run the tests
 * `benchmark/run` run the included timing benchmarks
 * `console` load a scala REPL with archery on the classpath.

Tests are written with [ScalaTest](http://www.scalatest.org/) and use the
excellent [ScalaCheck](https://github.com/rickynils/scalacheck) library for
automated specification-based testing.

The benchmarks are written against Rex Kerr's excellent library
[Thyme](https://github.com/Ichoran/thyme).

### License

Archery is available to you under the MIT license. See the `COPYING` file for
details.

### Credits

Archery is maintained by Erik Osheim.

Copyright (c) 2013 Meetup Inc.
