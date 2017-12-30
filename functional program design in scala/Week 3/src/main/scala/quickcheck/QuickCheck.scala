package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  lazy val genHeap: Gen[H] = for {
    item <- arbitrary[A]
    heap <- oneOf(const(empty), genHeap)
  } yield insert(item, heap)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("two elems and min") = forAll { (m1: Int, m2: Int) =>
    val smallest = m1 min m2
    val h = insert(m2, insert(m1, empty))
    findMin(h) == smallest
  }

  property("heap should be empty after remove last elem") = forAll { m: Int =>
    isEmpty(deleteMin(insert(m, empty)))
  }

  property("get sorted seq") = forAll { h: H =>
    def isSorted(h: H): Boolean =
      if (isEmpty(h)) true
      else {
        val m = findMin(h)
        val newH = deleteMin(h)
        isEmpty(newH) || (m <= findMin(newH) && isSorted(newH))
      }
    isSorted(h)
  }

  property("melding two heaps") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == (findMin(h1) min findMin(h2))
  }

  property("two heaps should be equal while removing mins") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val min1 = findMin(h1)
        val min2 = findMin(h2)
        min1 == min2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }

    heapEqual(
      meld(h1, h2),
      meld(deleteMin(h1), insert(findMin(h1), h2))
    )
  }
}
