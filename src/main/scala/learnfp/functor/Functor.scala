package learnfp.functor

trait Functor[F[_]] {
  def fmap[A, B](fa: F[A])(f: A => B): F[B]
}

class FunctorOps[A, F[_]](fa: F[A])(implicit functor: Functor[F]) {
  def fmap[B](f: A => B): F[B] = functor.fmap(fa)(f)
  def map[B](f: A => B): F[B] = functor.fmap(fa)(f) // to be compatible with for
}

class FxFunctorOps[A, B](f: A => B) {
  def `<$>`[F[_]](fa: F[A])(implicit functor: Functor[F]): F[B] = {
    functor.fmap(fa)(f)
  }
}

object FunctorOps {
  implicit def toFunctorOps[A, F[_]: Functor](fa: F[A]): FunctorOps[A, F] =
    new FunctorOps(fa)

  implicit def fxToFunctorOps[A, B](f: A => B): FxFunctorOps[A, B] =
    new FxFunctorOps[A, B](f)
}
