package learnfp.functor

import org.scalatest.{Matchers, WordSpecLike}
import learnfp.functor.StateInstance._

class StateTest extends WordSpecLike with Matchers {
  "state functor" should {
    "put works" in {
      State.put(20).run(10) shouldBe (20, ())
    }

    "get works " in {
      State.get.eval(10) shouldBe 10
    }

    "fmap works" in {
      val s = State[String, Int](s => ("extended " + s, 10))
      val ms = s.fmap(_ + 20)

      ms.run("state") shouldBe ("extended state", 30)
    }

    "obey identity" in {
      val s = State[String, Int](s => (s, 10))
      val ms = s.fmap(identity)

      ms.run("state") shouldBe ("state", 10)
    }

    "obey composition" in {
      val f1 = { x: Int =>
        x + 2
      }

      val f2 = { x: Int =>
        x * 2
      }

      val s1 = State[String, Int](s => (s, 10)).fmap(f1).fmap(f2)
      val s2 = State[String, Int](s => (s, 10)).fmap(f1.andThen(f2))

      s1.run("state") shouldBe s2.run("state")
    }
  }
}
