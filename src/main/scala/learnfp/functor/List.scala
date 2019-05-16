package learnfp.functor

object ListInstance {
  implicit val listInstance: Functor[List] = new Functor[List] {
    override def map[A, B](a: List[A])(fx: A => B): List[B] = ???
  }
}
