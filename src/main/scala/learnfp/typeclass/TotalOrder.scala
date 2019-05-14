package learnfp.typeclass

trait TotalOrder[A] {
  def less(lhs: A, rhs: A): Boolean
}

object TotalOrderInstances {
  implicit val intInstance: TotalOrder[Int] = (lhs: Int, rhs: Int) => lhs < rhs

  implicit val stringInstance: TotalOrder[String] =
    (lhs: String, rhs: String) => lhs.toInt < rhs.toInt

  implicit def listInstance[T](
      implicit suborder: TotalOrder[T]
  ): TotalOrder[List[T]] =
    (lhs: List[T], rhs: List[T]) =>
      lhs.zip(rhs).forall(t => suborder.less(t._1, t._2))
}

object Comparator {
  def less[A](lhs: A, rhs: A)(implicit order: TotalOrder[A]): Boolean = {
    order.less(lhs, rhs)
  }
}
