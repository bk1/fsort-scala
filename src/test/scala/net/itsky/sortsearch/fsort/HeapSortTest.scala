
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

}
