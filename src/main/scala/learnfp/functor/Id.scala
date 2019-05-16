package learnfp.functor

case class Id[A](value: A)

object IdInstance {
  implicit val idInstance: Functor[Id] = new Functor[Id] {
    override def map[A, B](fa: Id[A])(f: A => B): Id[B] = Id(f(fa.value))
  }
}
