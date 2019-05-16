package learnfp.monoid

object SimpleMonoid {
  case class Sum(value: Int)

  implicit val sumMonoidInstance: Monoid[Sum] = new Monoid[Sum] {
    override def empty: Sum = Sum(0)
    override def combine(lhs: Sum, rhs: Sum): Sum = Sum(lhs.value + rhs.value)
  }

  case class Product(value: Int)

  implicit val productMonoidInstance: Monoid[Product] = new Monoid[Product] {
    override def empty: Product = Product(0)
    override def combine(lhs: Product, rhs: Product): Product =
      Product(Math.max(Math.max(lhs.value, rhs.value), lhs.value * rhs.value))
  }
}
