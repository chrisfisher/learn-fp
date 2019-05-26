package learnfp.monad

import learnfp.functor.Id
import learnfp.monad.IdInstance._
import learnfp.monad.MonadOps._
import org.scalatest.{Matchers, WordSpecLike}

class IdTest extends WordSpecLike with Matchers {
  "id monad" should {
    "work" in {
      val initial: Id[Int] = 10.pure[Id]
      val expected: Id[Int] = 20.pure[Id]

      def plusTen(x: Int): Id[Int] = Id(x + 10)

      (initial >>= plusTen) shouldBe expected
    }
  }
}
