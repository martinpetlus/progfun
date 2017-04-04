package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      v <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(v, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == (a min b)
  }

  property("min3") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h)) == true
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { h: H =>
    def checkMinOrder(hh: H, prev: Int): Boolean =
      if (isEmpty(hh)) true
      else if (prev > findMin(hh)) false
      else checkMinOrder(deleteMin(hh), findMin(hh))

    if (isEmpty(h)) true
    else checkMinOrder(deleteMin(h), findMin(h))
  }

  property("gen3") = forAll { (h1: H, h2: H) =>
    val min1 = if (isEmpty(h1)) 0 else findMin(h1)
    val min2 = if (isEmpty(h2)) 1 else findMin(h2)
    val h11 = insert(min1, h1)
    val h22 = insert(min2, h2)
    findMin(meld(h11, h22)) == (min1 min min2)
  }

}
