## Archery

### Overview

Archery is a two-dimensional [R-Tree](http://en.wikipedia.org/wiki/R-tree)
implementation written in Scala. This README will contain more information
very soon.

### Contributing

Building this project requires SBT 0.13.0.

After you launch SBT, you can run the following commands:

 * `compile`: compile the project
 * `core/test`: run the tests
 * `benchmark/run`: run the included timing benchmarks
 * `console`: load a scala REPL with archery on the classpath.

Tests are written with [ScalaTest](http://www.scalatest.org/) and use the
excellent [ScalaCheck](https://github.com/rickynils/scalacheck) library for
automated specification-based testing.

The benchmarks are written against Rex Kerr's excellent library
[Thyme](https://github.com/Ichoran/thyme).

### License

Archery is available to you under the MIT license. See the `COPYING` file for
details.

### Credits

Archery is maintained by by Erik Osheim.

Copyright (c) 2013 Meetup Inc.
