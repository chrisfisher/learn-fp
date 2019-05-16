package learnfp.monoid

object PairAdditiveMonoid {
  case class Pair[A, B](a: A, b: B)

  implicit def nestedMonoidInstance[A, B](
      implicit aMonoid: Monoid[A],
      bMonoid: Monoid[B]
  ): Monoid[Pair[A, B]] =
    new Monoid[Pair[A, B]] {
      override def empty: Pair[A, B] = Pair(aMonoid.empty, bMonoid.empty)
      override def combine(lhs: Pair[A, B], rhs: Pair[A, B]): Pair[A, B] =
        Pair(
          aMonoid.combine(lhs.a, rhs.a),
          bMonoid.combine(lhs.b, rhs.b)
        )
    }
}
