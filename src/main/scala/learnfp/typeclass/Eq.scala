package learnfp.typeclass

trait Eq[A] {
  def eq(lhs: A, rhs: A): Boolean
}

object Eq {
  def eq[A](lhs: A, rhs: A)(implicit eqt: Eq[A]): Boolean = eqt.eq(lhs, rhs)
}

class EqOps[A](lhs: A)(implicit eqt: Eq[A]) {
  def ====(rhs: A): Boolean = eqt.eq(lhs, rhs)
}

object EqOps {
  implicit def toEqOps[A](lhs: A)(implicit eqt: Eq[A]): EqOps[A] =
    new EqOps(lhs)
}

object EqInstances {
  implicit val intEqInstance: Eq[Int] = (lhs: Int, rhs: Int) => lhs == rhs

  implicit val stringEqInstance: Eq[String] = (lhs: String, rhs: String) =>
    lhs.contentEquals(rhs)

  implicit def listEqInstance[A](implicit eqt: Eq[A]): Eq[List[A]] =
    (lhs: List[A], rhs: List[A]) =>
      lhs.length == rhs.length &&
        lhs.zip(rhs).forall(t => eqt.eq(t._1, t._2))
}
