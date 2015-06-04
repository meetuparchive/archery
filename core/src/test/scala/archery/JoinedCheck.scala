package archery

import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import Check._

class JoinedCheck extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

  property("laws for one value") {
    forAll { (j1: Joined[Int]) =>
      j1.isEmpty shouldBe j1.iterator.isEmpty
      val v1 = j1.toVector
      j1 should not be v1
      j1 should not be 12345
      j1 should not be "random string value"
      val w1 = Joined.wrap(v1)
      w1 shouldBe j1
      w1.## shouldBe j1.##
      (j1 ++ j1) shouldBe Joined.wrap(v1 ++ v1)
    }
  }

  property("laws for two values") {
    forAll { (j1: Joined[Int], j2: Joined[Int]) =>
      val v1 = j1.toVector
      val v2 = j2.toVector
      val j3 = j1 ++ j2
      val v3 = v1 ++ v2
      val w3 = Joined.wrap(v3)
      j3.isEmpty shouldBe (j1.isEmpty && j2.isEmpty)
      j3 shouldBe w3
      j3.## shouldBe w3.##
      v3 shouldBe w3.toVector
      // "occasional" false positives possible
      (j1.## == j2.##) shouldBe (j1 == j2)
    }
  }
}
