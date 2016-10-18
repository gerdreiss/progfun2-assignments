package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(k, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  lazy val genNonEmpty: Gen[H] = for {
    h <- arbitrary[H]
    x <- arbitrary[Int]
  } yield if (isEmpty(h)) insert(x, h) else h


  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    val ltm = m - 1
    findMin(insert(ltm, insert(m, h))) == ltm
  }

  property("gen3") = forAll { (h: H) =>
    deleteMin(insert(0, empty)) == empty
  }

  def findDeleteMin(h: H): List[Int] =
    if (isEmpty(h)) List()
    else findMin(h) :: findDeleteMin(deleteMin(h))

  def isSorted(res: Seq[Int]): Boolean = res match {
    case Nil => true
    case x :: Nil => true
    case x :: xs => x <= xs.head && isSorted(xs)
  }

  property("gen4") = forAll { (h: H) =>
    val res: Seq[Int] = findDeleteMin(h)
    isSorted(res)
  }

  property("gen5") = forAll(genNonEmpty,genNonEmpty) { (h1: H, h2: H) =>
    val m = Math.min(findMin(h1), findMin(h2))
    val h3 = meld(h1, h2)
    findMin(h3) == m
  }

}
