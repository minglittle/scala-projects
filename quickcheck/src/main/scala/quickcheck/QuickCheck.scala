package quickcheck

import com.sun.xml.internal.fastinfoset.sax.Properties
import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(value(empty), genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("ins2") = forAll { (a1: Int, a2: Int) =>
    val h = insert(a2, insert(a1, empty))
    val smallest = if (a1 < a2) a1 else a2
    findMin(h) == smallest
  }

  property("insD") = forAll { a: Int =>
    val h = insert(a, empty)
    val h1 = deleteMin(h)
    h1 == empty
  }

  property("fdMin") = forAll { h: H =>
    def fdMin(hs: H, h2: List[Int]): List[Int] = {
      if (isEmpty(hs)) h2 else findMin(hs) :: fdMin(deleteMin(hs), h2)
    }
    val xs = fdMin(h, Nil)
    xs == xs.sorted
  }

  property("meldM") = forAll { (h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    val m = meld(h1, h2)
    val minMeld = findMin(m)
    minMeld == min1 || minMeld == min2
  }
}
