// (C) Karl Brodowsky IT Sky Consulting GmbH 2019
// GNU-LGPL (see LICENSE in the root directory of the project)

package net.itsky.sortsearch.fsort

import java.util.Arrays
import scala.collection.mutable.IndexedSeq
import scala.reflect.ClassTag

object HeapSort {

  def hsort[T](unsorted : IndexedSeq[T], compare: Ordering[T])(implicit classTag: ClassTag[T]) : Unit = {
    hsortPartial(unsorted, 0, unsorted.length, compare)
  }

  def parent(idx : Int, start : Int) : Int = {
    (idx + start - 1) >> 1
  }

  def leftChild(idx : Int, start : Int) : Int = {
    idx * 2 + 1 - start
  }

  def rightChild(idx : Int, start : Int) : Int = {
    idx * 2 + 2 - start
  }

  def swapElements[T](arr  : IndexedSeq[T],
                      idx1 : Int,
                      idx2 : Int) : Unit = {
    val swap = arr(idx1)
    arr(idx1) = arr(idx2)
    arr(idx2) = swap
  }

  /**
   *  @param arr is the array that we are working on
   *  @param absoluteStart is the index of the lowest array element that is part of our sorting process (reasonable default 0)
   *  @param absoluteEnd is the index of the highest array element that is no longer part of our sorting process (reasonable default size of arr)
   *  @param relativeStart is the index where the area being processed in this step starts.  relativeStart >= absoluteStart
   */
  def siftDown[T](arr           : IndexedSeq[T],
                  absoluteStart : Int,
                  absoluteEnd   : Int,
                  relativeStart : Int,
                  compare       : Ordering[T]) : Unit = {
    println("siftDown(absolutStart=" + absoluteStart+ " absoluteEnd=" + absoluteEnd + " relativeStart=" + relativeStart + ")")
    if (relativeStart < absoluteStart) {
      throw new ArrayIndexOutOfBoundsException("relativeStart=" + relativeStart + " < absoluteStart=" + absoluteStart);
    }
    if (relativeStart >= absoluteEnd) {
      // nothing to do
      return
    }
    var rootIdx = relativeStart
    var running = true
    while (running) {
      val leftChildIdx = leftChild(rootIdx, absoluteStart)
      var swapIdx : Int = rootIdx
      if (leftChildIdx >= absoluteEnd) {
        // if left child is already leaving our range, right child would be outside as well.
        running = false
      } else {
        // check left child.  If it is greater than root, swap them
        println("siftDown(absolutStart=" + absoluteStart+ " absoluteEnd=" + absoluteEnd + " relativeStart=" + relativeStart + ") rootIdx=" + rootIdx + " leftChildIdx=" + leftChildIdx)
        if (compare.compare(arr(rootIdx), arr(leftChildIdx)) < 0) {
          swapIdx = leftChildIdx
        }
        val rightChildIdx = leftChildIdx + 1
        if (rightChildIdx < absoluteEnd) {
          // check right child.  If it is greater than root, swap them
          println("siftDown(absolutStart=" + absoluteStart+ " absoluteEnd=" + absoluteEnd + " relativeStart=" + relativeStart + ") rootIdx=" + rootIdx + " rightChildIdx=" + rightChildIdx)
          if (compare.compare(arr(swapIdx), arr(rightChildIdx)) < 0) {
            swapIdx = rightChildIdx
          }
        }
        if (swapIdx != rootIdx) {
          println("siftDown(absolutStart=" + absoluteStart+ " absoluteEnd=" + absoluteEnd + " relativeStart=" + relativeStart + ") rootIdx=" + rootIdx + " swapIdx=" + swapIdx)
          swapElements(arr, rootIdx, swapIdx)
          rootIdx = swapIdx
        } else {
          running = false
        }
      }
    }
  }

  def heapify[T](arr           : IndexedSeq[T],
                 absoluteStart : Int,
                 absoluteEnd   : Int,
                 compare       : Ordering[T]) : Unit = {
    println("heapify(absoluteStart=" + absoluteStart+ " absoluteEnd=" + absoluteEnd + ")")
    val maxIndex = absoluteEnd - 1
    val maxParent = parent(maxIndex, absoluteStart)
    for (idx <- maxParent to absoluteStart by -1) {
      siftDown(arr, absoluteStart, absoluteEnd, idx, compare)
    }
  }

  def hsortPartial[T](arr : IndexedSeq[T],
                      absoluteStart : Int, 
                      absoluteEnd : Int, 
                      compare: Ordering[T])(implicit classTag: ClassTag[T]) : Unit = {

    println("hsortPartial(absolutStart=" + absoluteStart+ " absoluteEnd=" + absoluteEnd + ")")
    if (absoluteEnd - absoluteStart > 1) {
      // only do non trivial sorting
      heapify(arr,
              absoluteStart,
              absoluteEnd,
              compare)
      for (idx <- (absoluteEnd-1) to (absoluteStart+1) by -1) {
        swapElements(arr, absoluteStart, idx)
        siftDown(arr, absoluteStart, idx, absoluteStart, compare)
      }
    }
  }
}
