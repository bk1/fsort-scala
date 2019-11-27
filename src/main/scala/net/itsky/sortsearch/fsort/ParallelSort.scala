package net.itsky.sortsearch.fsort

import java.util.concurrent.ExecutorService

import scala.concurrent.{ExecutionContext, Future}
import net.itsky.sortsearch.fsort.FlashSort.fsortWithFactor
import scala.concurrent.duration.Duration
import scala.concurrent.Await
import scala.concurrent.Awaitable
import scala.collection.mutable.IndexedSeq
import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuffer


object ParallelSort {

  private class SortedSeqConsumer[T](val sortedSeq : IndexedSeq[T], var pos : Int, val endPos : Int, val compare: Ordering[T])
    extends Comparable[SortedSeqConsumer[T]] {

    require(pos >= 0)
    require(pos <= endPos)
    require(endPos >= 0)
    require(endPos <= sortedSeq.size

    def peek : Option[T]  = {
      if (pos < endPos) {
        Some(sortedSeq(pos))
      } else {
        None
      }
    }

    def read : Option[T]  = {
      if (pos < endPos) {
        val ppos = pos
        pos += 1
        Some(sortedSeq(pos))
      } else {
        None
      }
    }

    def active : Boolean =  {
      pos < endPos
    }


    override def compareTo(other : SortedSeqConsumer[T]) : Int = {
      require(active)
      require(other.active)
      val my : T = peek.get
      val your : T = other.peek.get
      compare.compare(my, your)
    }
  }

  /** for fsort
    *
    * @param unsorted         the unsorted data. This needs to be mutable and is destroyed in the process :-( (TODO: fix this)
    * @param compare          the comparator
    * @param metric           the metric: like a hashCode but long and compatible with the ordering
    * @param nSegments        the number of parallel threads to use
    * @param executionContext thread pool
    * @param classTag         ensure that the type information is retained. It is needed internally
    * @tparam T type of the objects to be sorted
    * @return a sorted copy of the data
    */
  def fsortParallel[T](unsorted: IndexedSeq[T], compare: Ordering[T], metric: Function1[T, Long],
                       nSegments: Int, executionContext: ExecutionContext)
                      (implicit classTag: ClassTag[T]): IndexedSeq[T] = {

    if (nSegments <= 0) {
      throw new IllegalArgumentException("nSegments=" + nSegments + " needs to be greater than 0")
    }
    if (nSegments == 1) {
      return FlashSort.fsort(unsorted, compare, metric)(classTag)
    }
    val nSeq: Int = unsorted.size
    val step: Int = (nSeq + nSegments - 1) / nSegments
    if (step <= 2) {
      return FlashSort.fsort(unsorted, compare, metric)(classTag)
    }
    val segmentBoundaries : Seq[Int] = (0 to nSegments)
      .map(i => Math.min(i * step, nSeq))
    val segRange: Seq[Int] = (0 until nSegments).to[List[Int]]
    val futures: Seq[Future[Unit]] = segRange.map(
      seqIdx => Future[Unit]({
        FlashSort.fsortPartial(unsorted, segmentBoundaries(seqIdx), segmentBoundaries(seqIdx+1), compare, metric)
        return
      })(executionContext))
    // barrier: wait for all futures to complete
    val aggr = Future.sequence(futures)
    Await.result(aggr, Duration.Inf)
    val result : IndexedSeq[T] = new ArrayBuffer[T](nSeq)
    // create heap
    // TODO: merge the seqments
  }

  /** for non-fsort (currently only hsort supported) */
  def hsort[T](unsorted: IndexedSeq[T], compare: Ordering[T], metric: Function1[T, Long])(implicit classTag: ClassTag[T]): IndexedSeq[T] = {
    fsortWithFactor(unsorted, compare, metric, 0.42)(classTag)
  }
}
