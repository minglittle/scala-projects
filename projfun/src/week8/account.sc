import com.sun.corba.se.impl.ior.WireObjectKeyTemplate

object account {
  println("Welcome to the Scala worksheet")

  trait Parameter {
    def InverterDelay = 2
    def AndGateDelay = 3
    def OrGateDelay = 5
  }
  object sim extends Circuits with Parameter
  trait Simulation {
    type Action = () => Unit
    case class Event(time: Int, action: Action)
    private type Agenda = List[Event]
    private var agenda: Agenda = List()
    private var curtime = 0
    private def insert(ag: List[Event], item: Event): List[Event] = ag match {
      case first :: rest if first.time <= item.time =>
        first :: insert(rest, item)
      case _ => item :: ag
    }
    private def loop(): Unit = agenda match {
      case first :: rest =>
        agenda = rest
        curtime = first.time
        first.action()
        loop()
      case Nil =>
    }
    def currentTime: Int = curtime
    def currentTime: Int = ???
    def afterDelay(delay: Int)(block: => Unit): Unit = {
      val item = Event(curtime + delay, () => block)
      agenda = insert(agenda, item)
    }
    def run(): Unit = {
      afterDelay(0) {
        println("*** simulation started, time = " + currentTime + " ***")
      }
      loop()
    }
    def probe(name: String, wire: Wire): Unit = {
      def probeAction(): Unit = {
        // name + " " + currentTime + " value = " + wire.getSignal
        println(s"$name $currentTime value = ${wire.getSignal}")
      }
      wire addAction probeAction
    }
  }
  class Wire {
    private var sigVal = false
    private var actions: List[Action] = List()
    def getSignal: Boolean = sigVal
    def setSignal(s: Boolean): Unit =
      if (s != sigVal) {
        sigVal = s
        actions foreach (_())
      }
    def addAction(a: Action): Unit = {
      actions = a :: actions
      a()
    }
  }
  def inverter(input: Wire, output: Wire): Unit = {
    def invertAction(): Unit = {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output setSignal !inputSig }
    }
    input addAction invertAction
  }
  def andGate(in1: Wire, in2: Wire, output: Wire): Unit = {
    def andAction(): Unit = {
      val in1Sig = in1.getSignal
      val in2Sig = in2.getSignal
      afterDelay(AndGateDelay) { output setSignal (in1Sig & in2Sig) }
    }
    in1 addAction andAction
    in2 addAction andAction
  }
  def orGate(in1: Wire, in2: Wire, output: Wire): Unit = {
    def orAction(): Unit = {
      val in1Sig = in1.getSignal
      val in2Sig = in2.getSignal
      afterDelay(OrGateDelay) { output setSignal (in1Sig | in2Sig) }
    }
    in1 addAction orAction
    in2 addAction orAction
  }
  def halfAdder(a: Wire, b: Wire, s: Wire, c:Wire): Unit = {
    val d = new Wire
    val e = new Wire
    orGate(a, b, d)
    andGate(a, b, c)
    inverter(c, e)
    andGate(d, e, s)
  }

  val in1, in2, sum, carry = new Wire
  halfAdder(in1, in2, sum, carry)
  probe("sum", sum)
  probe("carry", carry)
  in1 setSignal true
  run()

  def power(x: Double, exp: Int): Double = {
    var r = 1.0
    var i = exp
    while (i > 0) { r = r * x; i = i - 1 }
    r
  }
  def WHILE(condition: => Boolean)(command: => Unit): Unit =
    if (condition) {
      command
      WHILE(condition)(command)
    }
    else ()
  def REPEAT(command: => Unit)(condition: => Boolean): Unit = {
    command
    if (condition) ()
    else REPEAT(command)(condition)
  }
  for (i <- 1 until 3; j <- "abc") println(i.toString + ' ' + j.toString)

  class BankAccount {
    private var balance = 0

    def deposit(amount: Int): Unit = {
      if (amount > 0) balance = balance + amount
    }

    def withdraw(amount: Int): Unit = {
      if (amount > 0 && amount <= balance) {
        balance = balance - amount
        balance
      } else throw new Error("insufficient funds")
    }
  }
  val acct = new BankAccount
  acct deposit 90
  acct withdraw 20
}