package quickcheck

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

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h))==m
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    findMin(h2) == math.min(a,b)
  }

  property("insert") = forAll { a: Int =>
    val h = insert(a, empty)
    val h2 = deleteMin(h)
    isEmpty(h2)
  }

  property("minGen") = forAll {  (h1: H, h2: H) =>
    val m = meld(h1, h2)
    findMin(m) == findMin(h1) || findMin(m) == findMin(h2)
  }



  property("genSort") = forAll { (h: H) =>
    def isSorted(heap: H, prev: Int, sorted: Boolean): Boolean = {
      if (!sorted) sorted
      else if (!isEmpty(heap))
        {
          def min = findMin(heap)
          isSorted(deleteMin(heap), min, prev <= min)
        }
      else sorted
    }

    if (isEmpty(h)) true else isSorted(deleteMin(h), findMin(h), true)

  }

  lazy val genHeap: Gen[H] = for {
    e <- arbitrary[A]
    m <- oneOf(const(empty), genHeap)
  } yield insert(e, m)



  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
