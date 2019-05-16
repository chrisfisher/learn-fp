package learnfp.monoid

object ListMonoid {
  implicit def listMonoid[T]: Monoid[List[T]] = new Monoid[List[T]] {
    override def empty: List[T] = List()
    override def combine(lhs: List[T], rhs: List[T]): List[T] = lhs ++ rhs
  }
}
