## Archery

### Overview

Archery is a two-dimensional [R-Tree](http://en.wikipedia.org/wiki/R-tree)
written in Scala. The implementation is immutable: adding and removing points
from the tree produces a new tree, leaving the old one untouched. Due to
[structural sharing](http://en.wikipedia.org/wiki/Persistent_data_structure)
this operation is quite efficient.

The name "archery" is a corruption of the word "R-Tree".

[![travis-ci.org](https://api.travis-ci.org/meetup/archery.svg?branch=master)](https://travis-ci.org/meetup/archery?branch=master)
[![codecov.io](http://codecov.io/github/meetup/archery/coverage.svg?branch=master)](http://codecov.io/github/meetup/archery?branch=master)

### Getting Archery

Archery is published to [bintray](https://bintray.com/) using the
[bintray-sbt](https://github.com/sbt/sbt-bintray) plugin.
Archery is available for Scala 2.11 and Scala 2.12.

If you use SBT, you can include Archery via the following `build.sbt`
snippet:

```scala
resolvers += "bintray/meetup" at "http://dl.bintray.com/meetup/maven"

libraryDependencies += "com.meetup" %% "archery" % "0.4.1"
```

For Maven or Ivy, you'll use the same resolver URL but you'll need a
slightly different artifact name (the example is for Scala 2.12):

```
org=com.meetup
name=archery_2.12
rev=0.4.1
```

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
val results: Seq[Entry[String]] = tree3.search(bbox)

// we can also just ask how many matching entries exist
val n: Int = tree3.count(bbox)
assert(results.length == n)
```

### Contributing

If you find something that seems like a bug in Archery, or see
confusing behavior, or find a place where the documentation or library
could be better, please [open an issue](https://github.com/meetup/archery/issues).

Pull requests are gladly accepted. The preferred strategy is to open an
issue or pull request where the feature can be discussed. We can use
the PR to collaborate on, and will merge it once everyone agrees on the
change, the tests and docs are updated, etc.

### Building Archery

Building this project requires SBT 1.x.

After you launch SBT, you can run the following commands:

 * `compile` compile the project.
 * `test` run the tests.
 * `scalastyle` run the style-checking.
 * `benchmark/run` run the included timing benchmarks.
 * `console` load a scala REPL with archery on the classpath.

(Travis automatically runs `test` and `scalastyle`, so any issues
should also be detected when a pull request is opened.)

You can generate coverage statistics manually by running the following
command from the command-line:

```
$ sbt clean coverage test coverageReport
```

Open `core/target/scala-2.12/scoverage-report/index.html` in a web
browser to see local coverage.

Tests are written with [ScalaTest](http://www.scalatest.org/) and use
the excellent [ScalaCheck](https://github.com/rickynils/scalacheck)
library for automated specification-based testing.

Test coverage is measured using
[sbt-scoverage](https://github.com/scoverage/sbt-scoverage) and tracked
at [codecov.io](https://codecov.io/github/meetup/archery).

The benchmarks are written against Rex Kerr's excellent library
[Thyme](https://github.com/Ichoran/thyme).

### License

Archery is available to you under the MIT license. See the
[COPYING](COPYING) file for details.

### Credits

Archery was originally written and maintained by Erik Osheim.

Copyright (c) 2013-2019 Meetup Inc.
