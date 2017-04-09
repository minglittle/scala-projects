import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

object adventure {
  trait Adventure {
    def collectCoins(): List[Coin]
    def buyTreasure(coins: List[Coin]): Treasure
  }

  val adventure = Adventure()
  val coins = adventure.collectCoins()
  val treasure = adventure.buyTreasure(coins)
  trait Socket {
    def readFromMemory(): Array[Byte]
    def sendToEurope(packet: Array[Byte]): Future[Array[Byte]]
  }
  val socket = Socket()
  val packet: Future[Array[Byte]] = socket.readFromMemory()
  // block for 50,000 ns
  // only continue if there is no exception
  packet.onComplete {
    case Success(p) => {
      val confirmation: Future[Array[Byte]] = socket.sendToEurope(p)
    }
    case Failure(t) => ...
  }

  // block for 150,000,000 ns
  // only continue if there is no exception

  trait Future[T] {
    //def onComplete(callback: Try[T] => Unit)(implicit executor: ExecutionContext): Unit
    def onComplete(success: T => Unit, failed: Throwable => Unit): Unit
    def onComplete(callback: Observer[T]): Unit
  }
  trait Observer[T] {
    def onNext(value: T): Unit
    def onError(error: Throwable): Unit
  }
  // Start an asynchronous computation and returns a future object to which
  // you can subscribe to be notified when the future completes
  object Future {
    def apply(body: => T)(implicit context: ExecutionContext): Future[T]
  }
}