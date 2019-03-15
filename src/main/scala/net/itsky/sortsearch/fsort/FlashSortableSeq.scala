// (C) Karl Brodowsky IT Sky Consulting GmbH 2019
// GNU-LGPL (see LICENSE in the root directory of the project)

package net.itsky.sortsearch.fsort

import java.util.Arrays
import scala.collection.mutable.IndexedSeq
import scala.reflect.ClassTag

object FlashSortableSeq {

  private def fsortCalculateK(metricValue : Long, factor : Double, lsize : Int) : Int = {
    // calculate prod as factor*value, add a small delta for rounding, force it into the closed interval [0,lsize-1] using min and max
    val prodUnlimited : Double = factor * metricValue + 1e-9
    val prodNonNegative : Double = Seq(0, prodUnlimited).max
    val prodFloat : Double = Seq(prodNonNegative, lsize-1).min
    val result : Int = prodFloat.toInt
    if (prodFloat < 0 || Math.abs(prodFloat - result) > 1.5) {
      // this should not happen, because prodFloat was prepared in a way to avoid it...
      // TODO prove & test with extreme values
      throw new IllegalStateException("Overflow/Underflow of Int when casting from " + prodFloat);
    }
    return result;
  }

  def fsort[T](unsorted : IndexedSeq[T], compare: Ordering[T], metric: Function1[T, Long])(implicit classTag: ClassTag[T]) : IndexedSeq[T] = { // FlashSortableSeq[T] = {
    fsortWithFactor(unsorted, compare, metric, 0.42)(classTag)
  }

  def fsortWithFactor[T](unsorted : IndexedSeq[T], compare: Ordering[T], metric: Function1[T, Long], factor : Double)(implicit classTag: ClassTag[T]) : IndexedSeq[T] = { // FlashSortableSeq[T] = {

    val nsize : Int = unsorted.size

    // size <= 1: nothing needs to be sorted
    if (nsize <= 1) {
      return unsorted
    }

    // use fsortCalculateK for a purpose it has not been made for, but since it is identical with what is needed here it is correct
    // we want lsize to be <= nsize and >= 2
    val lsizeSmall : Int = fsortCalculateK(nsize, factor, nsize);
    val lsize : Int = Seq(2, lsizeSmall).max
    val l : IndexedSeq[Int] = Array.fill(lsize){0}

    // use compare function for min and max by putting it into an implicit val
    implicit val cmp : Ordering[T] = compare;

    // find minimum of self (using cmp)
    val amin = unsorted.min
    // find maximum of self (using cmp)
    val amax = unsorted.max

    // we sort based on compare and not based on ==.
    // so it is safer to check if amin and amax are the same in terms of compare.  Then it is already sorted
    if (compare.compare(amin, amax) == 0) {
      return unsorted;
    }

    val aminMetric = metric(amin)
    val amaxMetric = metric(amax)

    // the metric does not differentiate our values. Then we have to fall back to Scala's built in sort method.
    if (aminMetric == amaxMetric) {
      return unsorted.sorted(cmp)
    }
    
    val step : Double = ((lsize - 1) : Double) / (amaxMetric - aminMetric)

    // iterate through self
    unsorted.foreach ( x =>  {
      val xMetric = metric(x)
      val k : Int = fsortCalculateK(xMetric - aminMetric, step, lsize)
      l(k) += 1
      Unit
    })
    println("l=" + l)
    
    val ll : IndexedSeq[Int] = l.scanLeft(0)(_+_)
    println("ll=" + ll)

    val result = new Array[T](nsize)
    
    val positions = ll.map(identity)

    unsorted.foreach ( x => {
      val k = fsortCalculateK(metric(x) - aminMetric, step, lsize)
      val pos = positions(k)
      result(pos) = x
      positions(k) += 1
      Unit
    })

    ll.sliding(2).foreach {
      case Seq(start, end) => {
        if (start < end - 1) {
          
         // TODO Arrays.sort[T](result, start, end) //, cmp)
          println("start=" + start + " end=" + end + " result=" + result)
        }
      }
    }
    // ll.shift
    // ll.inject(0) do |prev, current|
    //   result[prev..current] = result[prev..current].sort
    //   current
    // end

    // result

    return result
  }
}
