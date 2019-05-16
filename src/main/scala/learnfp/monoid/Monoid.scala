package learnfp.monoid

trait Monoid[A] {
  def combine(lhs: A, rhs: A): A
  def empty: A
}

object Monoid {
  def empty[A](implicit monoid: Monoid[A]): A = monoid.empty
}

class MonoidOps[A](lhs: A)(implicit monoid: Monoid[A]) {
  def |+|(rhs: A): A = monoid.combine(lhs, rhs)
}

object MonoidOps {
  implicit def toMonoidOps[A](x: A)(implicit monoid: Monoid[A]): MonoidOps[A] =
    new MonoidOps[A](x)
}
