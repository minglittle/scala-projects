import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object creatingFuture {
  val memory = Queue[EMailMessage](
    EMailMessage(from = "Erik", to = "Roland"),
    EMailMessage(from = "Martin", to = "Erik"),
    EMailMessage(from = "Roland", to = "Martin"))
  def readFromMemory(): Future[Array[Byte]] = Future {
    val email = queue.dequeue()
    val serializer = serialization.findSerializerFor(email)
    serializer.toBinary(email)
  }
  trait Awaitable[T] extends AnyRef {
    abstract def ready(atMost: Duration): Unit
    abstract def result(atMost: Duration): T
  }
  trait Future[T] extends Awaitable[T] {
    def filter(p: T => Boolean): Future[T]
    def flatMap[S](f: T => Future[S]): Future[U]
    def map[S](f: T => S): Future[S]
    def recoverWith(f: PartialFunction[Throwable, Future[T]]): Future[T]
  }
  object Future {
    def apply[T](body: => T): Future[T]
  }
  val socket = Socket()
  val packet: Future[Array[Byte]] = socket.readFromMemory()
  val confirmation: Future[Array[Byte]] = packet.flatMap(p => socket.sendToEurope(p))
}