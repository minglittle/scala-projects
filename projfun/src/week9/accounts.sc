import com.sun.crypto.provider.AESCipher.AES128_CBC_NoPadding

import scala.collection.mutable

object accounts {
  trait Subscriber {
    def handler(pub: Publisher)
  }
  trait Publisher {
    private var subscribers: Set[mutable.Subscriber] = Set()
    def subscribe(subscriber: mutable.Subscriber): Unit = subscribers += subscriber
    def unsubscribe(subscriber: mutable.Subscriber): Unit = subscribers -= subscriber
    def publish(): Unit = subscribers.foreach(_.handler(this))
  }
  class BankAccount extends Publisher {
    val balance = Var(0)
    def currentBalance: Int = balance()
    def deposit(amount: Int): Unit = {
      if (amount > 0) {
        val b = balance()
        balance() = b + amount
        publish()
      }
    }
    def withdraw(amount: Int): Unit = {
      if (amount > 0 && amount <= balance()) {
        val b = balance()
        balance() = b - amount
        publish()
      } else throw new Error("insufficient funds")
    }
  }
  def consolidated (accts: List[BankAccount]): Signal[Int] =
    Signal(accts.map(_.balance()).sum)
  val a = new BankAccount()
  val b = new BankAccount()
  val c = consolidated(List(a,b))
  c()
  a deposit 20

  class StackableVariable[T](init: T) {
    private var values: List[T] = List(init)
    def value: T = alues.head
    def withValue[R](newValue: T)(op: => R): R = {
      values = newValue :: values
      try op finally values = values.tail
    }
  }
  val caller = new StackableVar(initialSig)
  caller.withValue(otherSig) { ... }
  ... caller.value ...

  class Var[T](expr: => T) extends Signal[T](expr) {
    override def update(expr: => T): Unit = super.update(expr)
  }
  object Var {
    def apply[T](expr: => T) = new Var(expr)
  }
  object NoSignal extends Signal[Nothing](???) {
    override def computeValue() = ()
  }
  object Signal {
    private val caller = new StackableVariable[Signal[_]](NoSignal)
    def apply[T](expr: => T) = new Signal(expr)
  }
  class Signal[T](expr: => T) {
    import Signal._
    private var myExpr: () => T = _
    private var myValue: T = _
    private var observers: Set[Signal[_]] = Set()
    update(expr)
  }
  protected def update(expr: => T): Unit = {
    myExpr = () => expr
    computeValue()
  }
  protected def computeValue(): Unit = {
    myValue = caller.withValue(this)(myExpr())
  }
  def apply() = {
    observers += caller.value
    assert(!caller.value.observers.contains(this), "cyclic signal definition")
    myValue
  }
  protected def computeValue(): Unit = {
    val newValue = caller.withValue(this)(myExpr())+
    if (myValue != newValue) {
      myValue = newValue
      val obs = observers
      observers = Set()
      obs.foreach(_.computeValue())
    }
  }
}