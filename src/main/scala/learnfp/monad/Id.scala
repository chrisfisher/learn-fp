package learnfp.monad

import learnfp.functor.Id

object IdInstance {
  import learnfp.functor.IdInstance._

  implicit val monadIdInstance = new Monad[Id] {
    override def pure[A](a: A): Id[A] = Id(a)

    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa.value)
  }
}
