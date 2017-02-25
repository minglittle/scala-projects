package week4

import com.sun.jndi.toolkit.ctx.HeadTail
import sun.invoke.empty.Empty

/**
  * Created by Richard Wu on 2/19/17.
  */

trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  // def prepend(elem: T): List[T] = new Cons(elem, this)
  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)

}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def  isEmpty = false
}

object Nil extends List[Nothing] {
  def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException("Nil.head")

  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}
object test {
  val x: List[String] = Nil
  def f(xs: List[NonEmpty], x: Empty) = xs prepend x
  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
  }
  def isort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, isort(ys))
  }

}