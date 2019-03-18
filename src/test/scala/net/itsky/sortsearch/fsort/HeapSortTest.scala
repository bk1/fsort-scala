
package net.itsky.sortsearch.fsort

import java.util.Arrays
import scala.collection.mutable.IndexedSeq
import scala.reflect.ClassTag

import collection.mutable.Stack
import org.scalatest._

class HeapSortTest extends FlatSpec with Matchers {

  "An empty Array" should "be sorted without any changes" in {
    val arr : Array[Int] = Array()
    val cmp = Ordering.Int
    HeapSort.hsort(arr, cmp)
    arr.length should be (0)
  }

  "An empty segment of an Array at the beginning" should "be sorted without any changes" in {
    val arr : Array[Int] = Array(100, 20, 3)
    val cmp = Ordering.Int
    HeapSort.hsortPartial(arr, 0, 0, cmp)
    arr.length should be (3)
    arr(0) should be (100)
    arr(1) should be (20)
    arr(2) should be (3)
  }

  "An empty segment of an Array in the middle" should "be sorted without any changes" in {
    val arr : Array[Int] = Array(100, 20, 3)
    val cmp = Ordering.Int
    HeapSort.hsortPartial(arr, 1, 1, cmp)
    arr.length should be (3)
    arr(0) should be (100)
    arr(1) should be (20)
    arr(2) should be (3)
  }

  "An empty segment of an Array at the end" should "be sorted without any changes" in {
    val arr : Array[Int] = Array(100, 20, 3)
    val cmp = Ordering.Int
    HeapSort.hsortPartial(arr, 2, 2, cmp)
    arr.length should be (3)
    arr(0) should be (100)
    arr(1) should be (20)
    arr(2) should be (3)
  }

  "An empty segment of an Array after the end" should "be sorted without any changes" in {
    val arr : Array[Int] = Array(100, 20, 3)
    val cmp = Ordering.Int
    HeapSort.hsortPartial(arr, 3, 3, cmp)
    arr.length should be (3)
    arr(0) should be (100)
    arr(1) should be (20)
    arr(2) should be (3)
  }

  "An empty segment of an Array out of bound" should "be sorted without any changes" in {
    val arr : Array[Int] = Array(100, 20, 3)
    val cmp = Ordering.Int
    HeapSort.hsortPartial(arr, 5, 5, cmp)
    arr.length should be (3)
    arr(0) should be (100)
    arr(1) should be (20)
    arr(2) should be (3)
  }

  "An one element Array" should "be sorted without any changes" in {
    val arr : Array[Int] = Array(77)
    val cmp = Ordering.Int
    HeapSort.hsort(arr, cmp)
    arr.length should be (1)
    arr(0) should be (77)
  }

  "An one element segment of an Array at the beginning" should "be sorted without any changes" in {
    val arr : Array[Int] = Array(100, 20, 3)
    val cmp = Ordering.Int
    HeapSort.hsortPartial(arr, 0, 1, cmp)
    arr.length should be (3)
    arr(0) should be (100)
    arr(1) should be (20)
    arr(2) should be (3)
  }

  "An one element segment of an Array in the middle" should "be sorted without any changes" in {
    val arr : Array[Int] = Array(100, 20, 3)
    val cmp = Ordering.Int
    HeapSort.hsortPartial(arr, 1, 2, cmp)
    arr.length should be (3)
    arr(0) should be (100)
    arr(1) should be (20)
    arr(2) should be (3)
  }

  "An one element segment of an Array at the end" should "be sorted without any changes" in {
    val arr : Array[Int] = Array(100, 20, 3)
    val cmp = Ordering.Int
    HeapSort.hsortPartial(arr, 2, 3, cmp)
    arr.length should be (3)
    arr(0) should be (100)
    arr(1) should be (20)
    arr(2) should be (3)
  }

  "An two element Array duplicating one element" should "be sorted without any changes" in {
    val arr : Array[Int] = Array(79, 79)
    val cmp = Ordering.Int
    HeapSort.hsort(arr, cmp)
    arr.length should be (2)
    arr(0) should be (79)
    arr(1) should be (79)
  }

  "An two element ascending Array" should "be sorted without any changes" in {
    val arr : Array[Int] = Array(11, 12)
    val cmp = Ordering.Int
    HeapSort.hsort(arr, cmp)
    arr.length should be (2)
    arr(0) should be (11)
    arr(1) should be (12)
  }

  "An two element descending Array" should "be sorted ascendingly" in {
    val arr : Array[Int] = Array(12, 11)
    val cmp = Ordering.Int
    HeapSort.hsort(arr, cmp)
    arr.length should be (2)
    arr(0) should be (11)
    arr(1) should be (12)
  }

  "An two element descending start of an Array" should "be sorted ascendingly" in {
    val arr : Array[Int] = Array(12, 11, -202, -303)
    val cmp = Ordering.Int
    HeapSort.hsortPartial(arr, 0, 2, cmp)
    arr.length should be (4)
    arr(0) should be (11)
    arr(1) should be (12)
    arr(2) should be (-202)
    arr(3) should be (-303)
  }

  "An two element descending middle of an Array" should "be sorted ascendingly" in {
    val arr : Array[Int] = Array(1000, 12, 11, -303)
    val cmp = Ordering.Int
    HeapSort.hsortPartial(arr, 1, 3, cmp)
    arr.length should be (4)
    arr(0) should be (1000)
    arr(1) should be (11)
    arr(2) should be (12)
    arr(3) should be (-303)
  }

  "An two element descending end of an Array" should "be sorted ascendingly" in {
    val arr : Array[Int] = Array(1000, 101, 12, 11)
    val cmp = Ordering.Int
    HeapSort.hsortPartial(arr, 2, 4, cmp)
    arr.length should be (4)
    arr(0) should be (1000)
    arr(1) should be (101)
    arr(2) should be (11)
    arr(3) should be (12)
  }

  "An three element Array" should "be sorted ascendingly" in {
    val arrs : Seq[Array[Int]] = Seq(Array(1, 2, 3), Array(2, 1, 3), Array(1, 3, 2), Array(2, 3, 1), Array(3, 2, 1), Array(3, 1, 2))
    val cmp = Ordering.Int
    var arr = Array()
    for (arr <- arrs) {
      println("before=" + arr.to[Seq])
      HeapSort.hsort(arr, cmp)
      println("after=" + arr.to[Seq])
      arr.length should be (3)
      arr(0) should be (1)
      arr(1) should be (2)
      arr(2) should be (3)
    }
  }

  "An four element Array" should "be sorted ascendingly" in {
    val arrs : Seq[Array[Int]] = Seq(Array(1, 2, 3, 4), Array(2, 1, 4, 3), Array(1, 3, 4, 2), Array(2, 3, 4, 1), Array(4, 3, 2, 1), Array(3, 4, 1, 2))
    val cmp = Ordering.Int
    var arr = Array()
    for (arr <- arrs) {
      println("before=" + arr.to[Seq])
      HeapSort.hsort(arr, cmp)
      println("after=" + arr.to[Seq])
      arr.length should be (4)
      arr(0) should be (1)
      arr(1) should be (2)
      arr(2) should be (3)
      arr(3) should be (4)
    }
  }

  "An five element Array" should "be sorted ascendingly" in {
    val arrs : Seq[Array[Int]] = Seq(Array(1, 2, 3, 4, 5), Array(5, 4, 3, 2, 1), Array(1, 5, 2, 4, 3))
    val cmp = Ordering.Int
    var arr = Array()
    for (arr <- arrs) {
      println("before=" + arr.to[Seq])
      HeapSort.hsort(arr, cmp)
      println("after=" + arr.to[Seq])
      arr.length should be (5)
      arr(0) should be (1)
      arr(1) should be (2)
      arr(2) should be (3)
      arr(3) should be (4)
      arr(4) should be (5)
    }
  }

  "An six element Array" should "be sorted ascendingly" in {
    val arrs : Seq[Array[Int]] = Seq(Array(1, 2, 3, 4, 5, 6), Array(6, 5, 4, 3, 2, 1), Array(1, 5, 2, 6, 4, 3))
    val cmp = Ordering.Int
    var arr = Array()
    for (arr <- arrs) {
      println("before=" + arr.to[Seq])
      HeapSort.hsort(arr, cmp)
      println("after=" + arr.to[Seq])
      arr.length should be (6)
      arr(0) should be (1)
      arr(1) should be (2)
      arr(2) should be (3)
      arr(3) should be (4)
      arr(4) should be (5)
      arr(5) should be (6)
    }
  }

}
