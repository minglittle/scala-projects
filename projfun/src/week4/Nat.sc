package week4

// Peano Numbers
abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  def isZero = true
  def predecessor = throw new Error("0.predecessor")
  def + (that: Nat): Nat = this + that
  def - (that: Nat): Nat = if (that.isZero) this else throw new Error("negative number")
}

class Succ(n: Nat) extends Nat {
  def isZero = false
  def predecessor = n
  def + (that: Nat) = new Succ(n + that)
  def - (that: Nat) = if (that.isZero) this else n - that.predecessor
}

new Function1[Int,List] {
  def apply() = new Nil
  def apply(x: Int) = new Cons(x, new Nil)
  def apply(x1: Int, x2: Int) = new Cons(x1, new Cons(x2, new nil))
}

object List {
  // List(1, 2) = List.apply(1, 2)
  def apply[T](x1: T, x2: T) = new Cons(x1, new Cons(x2, new nil))
  def apply[T]() = new Nil
}

val a: Array[NonEmpty] = Array(new NonEmpty(1, Empty, Empty))
val b: Array[IntSet] = a
b(0) = Empty
val s: NonEmpty = a(0)

