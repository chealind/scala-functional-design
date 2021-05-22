package quickcheck

import org.scalacheck.Gen.const
import org.scalacheck.Prop._
import org.scalacheck._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = {
    for {
      values <- Gen.listOfN(100, Gen.choose(-1000, 1000))
      heap <- insertIn(values, empty)
    } yield heap
  }

  lazy val genList: Gen[List[Int]] = {
    for {
      values <- Gen.listOfN(100, Gen.choose(-1000, 1000))
    } yield values
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  implicit lazy val arbList: Arbitrary[List[Int]] = Arbitrary(genList)

  property("gen1") = forAll { (heap: H) =>
    val min = if (isEmpty(heap)) 0 else findMin(heap)
    findMin(insert(min, heap)) == min
  }

  property("insert an element into an empty heap, then find the minimum") = forAll { a: Int =>
    val heap = insert(a, empty)
    findMin(heap) == a
  }

  property("insert an element into an empty heap, then delete the minimum") = forAll { a: Int =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  property("insert any two elements into an empty heap, find the minimum") = forAll { (a: Int, b: Int) =>
    val min = if (a < b) a else b
    val heap = insert(b, insert(a, empty))
    findMin(heap) == min
  }

  property("minimum of the melding of any two heaps") = forAll { (heap1: H, heap2: H) =>
    val min1 = if (isEmpty(heap1)) 0 else findMin(heap1)
    val min2 = if (isEmpty(heap2)) 0 else findMin(heap2)
    val heap = meld(heap1, heap2)
    val min = if (isEmpty(heap)) 0 else findMin(heap)
    min == min1 || min == min2
  }

  property("get a sorted sequence of elements") = forAll { (heap: H) =>
    if (isEmpty(heap)) true
    else nextMin(findMin(heap), deleteMin(heap))
  }

  property("compare sorted list minimum") = forAll { (list: List[Int]) =>
    val sorted = list.sorted

    val original = insertIn(list, empty)

    val result = getList(original)

    sorted == result
  }

  @tailrec
  final def nextMin(min: A, heap: H): Boolean = {
    if (isEmpty(heap)) true
    else {
      val currentMin = findMin(heap)
      if (currentMin < min) false else nextMin(currentMin, deleteMin(heap))
    }
  }

  final def insertIn(values: List[Int], heap: H): H = values match {
    case Nil => heap
    case y :: ys => {
      val node = insert(y, heap)
      meld(node, insertIn(ys, heap))
    }
  }

  final def getList(heap: H): List[Int] = {
    if (isEmpty(heap)) List()
    else {
      val min = findMin(heap)
      val rest = deleteMin(heap)
      min :: getList(rest)
    }
  }
}
