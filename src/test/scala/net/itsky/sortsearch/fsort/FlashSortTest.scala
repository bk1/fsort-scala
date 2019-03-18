
package net.itsky.sortsearch.fsort

import org.scalatest._

class FlashSortTest extends FlatSpec with Matchers {

  val cmp = Ordering.Int
  val metric = (x : Int) => x.longValue() << 8

  "An empty Array" should "be sorted without any changes" in {
    val unsorted : Array[Int] = Array()
    val arr = FlashSort.fsort(unsorted, cmp, metric)
    arr.length should be (0)
  }

  "An one element Array" should "be sorted without any changes" in {
    val unsorted : Array[Int] = Array(77)
    val arr = FlashSort.fsort(unsorted, cmp, metric)
    arr.length should be (1)
    arr(0) should be (77)
  }

  "An two element Array duplicating one element" should "be sorted without any changes" in {
    val unsorted : Array[Int] = Array(79, 79)
    val arr = FlashSort.fsort(unsorted, cmp, metric)
    arr.length should be (2)
    arr(0) should be (79)
    arr(1) should be (79)
  }

  "An two element ascending Array" should "be sorted without any changes" in {
    val unsorted : Array[Int] = Array(11, 12)
    val arr = FlashSort.fsort(unsorted, cmp, metric)
    arr.length should be (2)
    arr(0) should be (11)
    arr(1) should be (12)
  }

  "An two element descending Array" should "be sorted ascendingly" in {
    val unsorted : Array[Int] = Array(12, 11)
    val arr = FlashSort.fsort(unsorted, cmp, metric)
    arr.length should be (2)
    arr(0) should be (11)
    arr(1) should be (12)
  }

  "An three element Array" should "be sorted ascendingly" in {
    val arrs : Seq[Array[Int]] = Seq(Array(1, 2, 3), Array(2, 1, 3), Array(1, 3, 2), Array(2, 3, 1), Array(3, 2, 1), Array(3, 1, 2))
    for (unsorted <- arrs) {
      val arr = FlashSort.fsort(unsorted, cmp, metric)
      arr.length should be (3)
      arr(0) should be (1)
      arr(1) should be (2)
      arr(2) should be (3)
    }
  }

  "An four element Array" should "be sorted ascendingly" in {
    val arrs : Seq[Array[Int]] = Seq(Array(1, 2, 3, 4), Array(2, 1, 4, 3), Array(1, 3, 4, 2), Array(2, 3, 4, 1), Array(4, 3, 2, 1), Array(3, 4, 1, 2))
    for (unsorted <- arrs) {
      val arr = FlashSort.fsort(unsorted, cmp, metric)
      arr.length should be (4)
      arr(0) should be (1)
      arr(1) should be (2)
      arr(2) should be (3)
      arr(3) should be (4)
    }
  }

  "An five element Array" should "be sorted ascendingly" in {
    val arrs : Seq[Array[Int]] = Seq(Array(1, 2, 3, 4, 5), Array(5, 4, 3, 2, 1), Array(1, 5, 2, 4, 3))
    for (unsorted <- arrs) {
      val arr = FlashSort.fsort(unsorted, cmp, metric)
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
    for (unsorted <- arrs) {
      val arr = FlashSort.fsort(unsorted, cmp, metric)
      arr.length should be (6)
      arr(0) should be (1)
      arr(1) should be (2)
      arr(2) should be (3)
      arr(3) should be (4)
      arr(4) should be (5)
      arr(5) should be (6)
    }
  }

  "An ten element Array" should "be sorted ascendingly" in {
    val arrs : Seq[Array[Int]] = Seq(Array(10, 8, 9, 4, 5, 6, 1, 2, 3, 7), Array(10, 9, 8, 7, 6, 5, 4, 3, 2, 1))
    for (unsorted <- arrs) {
      val arr = FlashSort.fsort(unsorted, cmp, metric)
      arr.length should be (10)
      arr(0) should be (1)
      arr(1) should be (2)
      arr(2) should be (3)
      arr(3) should be (4)
      arr(4) should be (5)
      arr(5) should be (6)
      arr(6) should be (7)
      arr(7) should be (8)
      arr(8) should be (9)
      arr(9) should be (10)
    }
  }

  "An ten element Array with duplicates" should "be sorted ascendingly" in {
    val arrs : Seq[Array[Int]] = Seq(Array(10, 8, 8, 4, 4, 6, 0, 2, 2, 6), Array(10, 8, 8, 6, 6, 4, 4, 2, 2, 0))
    for (unsorted <- arrs) {
      val arr = FlashSort.fsort(unsorted, cmp, metric)
      arr.length should be (10)
      arr(0) should be (0)
      arr(1) should be (2)
      arr(2) should be (2)
      arr(3) should be (4)
      arr(4) should be (4)
      arr(5) should be (6)
      arr(6) should be (6)
      arr(7) should be (8)
      arr(8) should be (8)
      arr(9) should be (10)
    }
  }

  "An thousand element Array" should "be sorted ascendingly" in {
    val unsorted = (1000 to 1 by -1).to[Array]
    unsorted.length should be (1000)
    unsorted(0) should be (1000)
    unsorted(999) should be (1)
    val arr = FlashSort.fsort(unsorted, cmp, metric)
    arr.length should be (1000)
    for (idx <- 0 to 999) {
      arr(idx) should be (idx + 1)
    }
  }

}
