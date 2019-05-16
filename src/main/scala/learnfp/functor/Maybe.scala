package learnfp.functor

object Maybe {
  sealed trait Maybe[+A]

  case class Just[A](value: A) extends Maybe[A]

  object Just {
    def map[A, B](fa: Just[A])(f: A => B): Just[B] = Just[B](f(fa.value))
  }

  case class Nothing[A]() extends Maybe[A]

  object Nothing {
    def map[A, B](fa: Nothing[A])(f: A => B): Nothing[B] = Nothing[B]()
  }

  def nothing[A]: Maybe[A] = Nothing[A]()

  def just[A](x: A): Maybe[A] = Just(x)
}

object MaybeInstance {
  import Maybe._

  implicit val maybeInstance: Functor[Maybe] = new Functor[Maybe] {
    override def map[A, B](fa: Maybe[A])(f: A => B): Maybe[B] = fa match {
      case Just(a) => just(f(a))
      case _       => nothing
    }
  }
}
