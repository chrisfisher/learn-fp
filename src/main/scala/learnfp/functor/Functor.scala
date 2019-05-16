package learnfp.functor

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

class FunctorOps[A, F[_]](fa: F[A])(implicit functor: Functor[F]) {
  def map[B](fx: A => B): F[B] = functor.map(fa)(fx)
}

class FxFunctorOps[A, B](f: A => B) {
  def `<$>`[F[_]](a: F[A])(implicit functor: Functor[F]): F[B] =
    functor.map(a)(f)
}

object FunctorOps {
  implicit def toFunctorOps[A, F[_]: Functor](fa: F[A]): FunctorOps[A, F] =
    new FunctorOps(fa)

  implicit def fxToFunctorOps[A, B](f: A => B): FxFunctorOps[A, B] =
    new FxFunctorOps[A, B](f)
}
