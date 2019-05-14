package learnfp.typeclass

trait TypeClass[A] {
  def foo(x:A):String
}

object TypeClassInstances {
  implicit val intInstance:TypeClass[Int] = (_: Int) => "int"

  implicit val stringInstance:TypeClass[String] = (_: String) => "string"
}

object TypeClassUser {
  def foo[A](x:A)(implicit typeClass:TypeClass[A]):String  = {
    typeClass.foo(x)
  }
}
