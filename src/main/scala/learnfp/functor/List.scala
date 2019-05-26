package learnfp.functor

object ListInstance {
  implicit val listInstance: Functor[List] = new Functor[List] {
    override def fmap[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }
}
