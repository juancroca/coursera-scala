package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
/*  lazy val genMap: Gen[Map[Int,Int]] = oneOf(
    const(Map.empty[Int,Int]),
    for {
      k <- arbitrary[Int]
      v <- arbitrary[Int]
      m <- oneOf(const(Map.empty[Int,Int]), genMap)
    } yield m.updated(k, v)
  )*/
  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for{
      v <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    } yield insert(v, h)

  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { a: Int =>
    val mina = if (a - 1 < a ) a - 1 else a
    val maxa = if (a - 1 < a ) a else a -1
    val h = insert(maxa, insert(mina, empty))
    findMin(h) == mina
  }

  property("del1") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("del2") = forAll { a: Int =>
    val h = insert(a+1, insert(a, empty))
    !isEmpty(deleteMin(h))
  }

  property("del3") = forAll { (ns: List[Int]) =>

    def dumpHeap(h: H): List[Int] = if(isEmpty(h)) Nil else findMin(h) :: dumpHeap(deleteMin(h))

    val h = ns.foldLeft(empty)((h,n) => insert(n,h))
    (ns.sortWith(_ < _), dumpHeap(h)).zipped.forall(_ == _)
  }

  property("meld1") = forAll { a: Int =>
    val h1 = insert(a+1, insert(a, empty))
    val h2 = insert(a-1, insert(a, empty))
    val h = meld(h1, h2)
    findMin(h) == findMin(h1) || findMin(h) == findMin(h2)
  }

  property("meld2") = forAll { a: Int =>
    val h1 = insert(a+1, insert(a, empty))
    val h2 = insert(a-1, insert(a, empty))
    val h = meld(h1, h2)
    !isEmpty(h)
  }

  property("meld3") = forAll { a: Int =>
    val h1 = insert(a+1, insert(a, empty))
    val h2 = insert(a-1, insert(a, empty))
    val h = meld(h1, h2)
    !isEmpty(deleteMin(h))
  }

  property("meld4") = forAll { a: Int =>
    val h1 =  insert(a, empty)
    val h = meld(h1, empty)
    isEmpty(deleteMin(h))
  }
}
