import scala.collection.mutable
import scala.reflect.api.Position

object myPublisher {
  println("Welcome to the Scala worksheet")
  trait Publisher {
    private var subscribers: Set[mutable.Subscriber] = Set()
    def subscribe(subscriber: mutable.Subscriber): Unit = subscribers += subscriber
    def unsubscribe(subscriber: mutable.Subscriber): Unit = subscribers -= subscriber
    def publish(): Unit = subscribers.foreach(_.handler(this))
  }
  trait Subscriber {
    def handler(pub: Publisher)
  }
  class BankAccount extends Publisher {
    val balance = Var(0)
    def currentBalance: Int = balance()
    def deposit(amount: Int): Unit = {
      if (amount > 0) {
        balance() = balance() + amount
        publish()
      }
    }
    def withdraw(amount: Int): Unit = {
      if (amount > 0 && amount <= balance()) {
        balance() = balance() - amount
        publish()
      } else throw new Error("insufficient funds")
    }
  }
  class Consolidator(observed: List[BankAccount]) extends Subscriber {
    observed.foreach(_.subscribe(this))
    private var total: Int = _ compute()
    private  def compute() = total = observed.map(_.currentBalance).sum
    def handler(pub: Publisher) = compute()
    def totalBalance = total
  }
  def inReactangle(LL: Position, UR: Position): Signal[Boolean] =
  Signal {
    val pos = mousePosition()
    LL <= pos && pos <= UR
  }
  val a = new BankAccount
  val b = new BankAccount
  val c = new consolidator(List(a, b))
  c.totalBalance
  a.deposit 20
  c.totalBalance
  b deposit 30
  c.totalBalance

  val sig = Var(3)
  sig.update(5)

}
