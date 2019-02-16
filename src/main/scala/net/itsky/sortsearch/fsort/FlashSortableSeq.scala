package net.itsky.sortsearch.fsort

import scala.collection.mutable.IndexedSeq

trait FlashSortableSeq[T] extends IndexedSeq[T] {

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

  def fsort(compare: Ordering[T], metric: Function1[T, Long]): FlashSortableSeq[T] = {
    fsortWithFactor(compare, metric, 0.42);
  }

  def fsortWithFactor(compare: Ordering[T], metric: Function1[T, Long], factor : Double): FlashSortableSeq[T] = {

    val nsize : Int = size

    // size <= 1: nothing needs to be sorted
    if (nsize <= 1) {
      return this
    }

    // use fsortCalculateK for a purpose it has not been made for, but since it is identical with what is needed here it is correct
    // we want lsize to be <= nsize and >= 2
    val lsizeSmall : Int = fsortCalculateK(nsize, factor, nsize);
    val lsize : Int = Seq(2, lsizeSmall).max
    val l : IndexedSeq[Int] = Array.fill(lsize){0}

    // use compare function for min and max by putting it into an implicit val
    implicit val cmp : Ordering[T] = compare;

    // find minimum of self (using cmp)
    val amin = min
    // find maximum of self (using cmp)
    val amax = max

    // we sort based on compare and not based on ==.
    // so it is safer to check if amin and amax are the same in terms of compare.  Then it is already sorted
    if (compare.compare(amin, amax) == 0) {
      return this;
    }

    val aminMetric = metric(amin)
    val amaxMetric = metric(amax)

    // the metric does not differentiate our values. Then we have to fall back to Scala's built in sort method.
    if (aminMetric == amaxMetric) {
      // sorted
      return this
    }
    
      val step : Double = ((lsize - 1) : Double) / (amaxMetric - aminMetric)

      // iterate through self
      foreach ( x =>  {
        val xMetric = metric(x) // fsortMetric(x, metric)
        val k : Int = fsortCalculateK(xMetric - aminMetric, step, lsize)
        l(k) = l(k) + 1
        Unit
      })
      // puts "x=//{x} x.metric=//{xMetric} step=//{step} k=//{k} l[k]=//{l[k]}"
    

    //puts "l=//{l.inspect}"

    // val ll = l.inject([0]) do |partial_list, entry|

    //   last_of_partial = partial_list.last
    //   partial_list << last_of_partial + entry
    // end
    // //puts "ll=//{l.inspect}"

    // result = [nil] * nsize
    
    // positions = ll.clone

    // each do |x|
    //   k = fsortCalculateK(step, fsortMetric(x, metric) - aminMetric, lsize)
    //   pos = positions[k]
    //   result[pos] = x
    //   positions[k] += 1
    // end
    // //puts "positions=//{positions.inspect}"
    // //puts("grouped: //{result.inspect}")

    // ll.shift
    // ll.inject(0) do |prev, current|
    //   result[prev..current] = result[prev..current].sort
    //   current
    // end

    // result

      return this
  }
}
