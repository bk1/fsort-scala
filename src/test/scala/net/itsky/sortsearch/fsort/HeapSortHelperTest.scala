
package net.itsky.sortsearch.fsort

import java.util.Arrays
import scala.collection.mutable.IndexedSeq
import scala.reflect.ClassTag

import collection.mutable.Stack
import org.scalatest._

class HeapSortHelperTest extends FlatSpec with Matchers {

  "parent of left child" should "be original" in {
    var orig: Int = 0
    var absoluteStart: Int = 0
    for (absoluteStart <- 0 to 20) {
      for (orig <- absoluteStart to 50) {
        val leftChildIdx = HeapSort.leftChild(orig, absoluteStart)
        val parentIdx = HeapSort.parent(leftChildIdx, absoluteStart)
        parentIdx should be(orig)
      }
    }
  }

  "parent of right child" should "be original" in {
    var orig: Int = 0
    var absoluteStart: Int = 0
    for (absoluteStart <- 0 to 20) {
      for (orig <- absoluteStart to 50) {
        val rightChildIdx = HeapSort.rightChild(orig, absoluteStart)
        val parentIdx = HeapSort.parent(rightChildIdx, absoluteStart)
        parentIdx should be(orig)
      }
    }
  }

  "rightChild and leftChild" should "be apart by 1" in {
    var orig: Int = 0
    var absoluteStart: Int = 0
    for (absoluteStart <- 0 to 20) {
      for (orig <- absoluteStart to 50) {
        val leftChildIdx = HeapSort.leftChild(orig, absoluteStart)
        val rightChildIdx = HeapSort.rightChild(orig, absoluteStart)
        rightChildIdx should be(leftChildIdx + 1)
      }
    }
  }

  "leftChild or rightChild of parent" should "be original" in {
    var orig: Int = 0
    var absoluteStart: Int = 0
    for (absoluteStart <- 0 to 20) {
      for (orig <- absoluteStart + 1 to 50) {
        val parentIdx = HeapSort.parent(orig, absoluteStart)
        val leftChildIdx = HeapSort.leftChild(parentIdx, absoluteStart)
        val rightChildIdx = HeapSort.rightChild(parentIdx, absoluteStart)
        Seq(leftChildIdx, rightChildIdx) should (contain(orig))
      }
    }
  }

}
