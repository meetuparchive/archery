package archery

import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import Check._

class GeomCheck extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

  property("point invariants") {
    forAll { (p: Point, g: Geom) =>
      p.x2 shouldBe p.x
      p.y2 shouldBe p.y
      p.height shouldBe 0F
      p.width shouldBe 0F
      p.area shouldBe 0F
      p.wraps(g) shouldBe false
    }
  }

  property("geom isFinite") {
    forAll { (x: Float, y: Float, x2: Float, y2: Float) =>
      def ok(n: Float): Boolean = !(n.isNaN || n.isInfinite)
      Point(x, y).isFinite shouldBe ok(x) && ok(y)
      Box(x, y, x2, y2).isFinite shouldBe ok(x) && ok(y) && ok(x2) && ok(y2)
    }
  }

  property("geom height") {
    forAll { (g: Geom) =>
      g.height should be >= 0F
      g.height shouldBe (g.y2 - g.y)
    }
  }

  property("geom width") {
    forAll { (g: Geom) =>
      g.width should be >= 0F
      g.width shouldBe (g.x2 - g.x)
    }
  }

  property("geom area") {
    forAll { (g: Geom) =>
      g.area should be >= 0F
      g.area shouldBe g.width * g.height
    }
  }

  property("geom edges") {
    forAll { (g: Geom) =>
      g.lowerLeft shouldBe Point(g.x, g.y)
      g.upperRight shouldBe Point(g.x2, g.y2)
    }
  }
}
