package net.itsky.sortsearch.fsort

import java.util.concurrent.ExecutorService

import scala.concurrent.{ExecutionContext, Future}
import net.itsky.sortsearch.fsort.FlashSort.fsortWithFactor

import scala.collection.{immutable, mutable}
import scala.concurrent.duration.Duration
import scala.concurrent.Await
import scala.concurrent.Awaitable
import scala.collection.mutable.IndexedSeq
import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuffer


object ParallelSort {

  private class SortedPartialSeqConsumer[T](val sortedSeq : IndexedSeq[T], var pos : Int, val endPos : Int, val compare: Ordering[T])
  extends Comparable[SortedPartialSeqConsumer[T]] {

    require(pos >= 0)
    require(pos <= endPos)
    require(endPos >= 0)
    require(endPos <= sortedSeq.size)

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
        Some(sortedSeq(ppos))
      } else {
        None
      }
    }

    def active : Boolean =  {
      pos < endPos
    }


    override def compareTo(other : SortedPartialSeqConsumer[T]) : Int = {
      require(active)
      require(other.active)
      val my : T = peek.get
      val your : T = other.peek.get
      compare.compare(my, your)
    }


  }

  private class HeapOfSortedPartialSeqs[T](val seq: IndexedSeq[T],
                                           val compare: Ordering[T],
                                           val nSegments: Int,
                                           val segmentBoundaries: Seq[Int]) {
    val reverseCompareSubSeq : Ordering[SortedPartialSeqConsumer[T]] = (x, y) => y.compareTo(x)
    val heapOfSegments : ArrayBuffer[SortedPartialSeqConsumer[T]]
        = new ArrayBuffer[SortedPartialSeqConsumer[T]](nSegments)
    for (i : Int <- (0 until nSegments)) {
      val startPos = segmentBoundaries(i)
      val endPos = segmentBoundaries(i+1)
      if (startPos < endPos) {
        val partialSeq = new SortedPartialSeqConsumer[T](seq, startPos, endPos, compare)
        heapOfSegments.append(partialSeq)
      }
    }

    HeapSort.heapify[SortedPartialSeqConsumer[T]](heapOfSegments, 0, heapOfSegments.size, reverseCompareSubSeq)

    def active : Boolean = {
      heapOfSegments.size > 0
    }

    def peek : Option[T] = {
      if (active) {
        heapOfSegments(0).peek
      } else {
        None
      }
    }

    def read : Option[T]  = {
      if (active) {
        val result = heapOfSegments(0).read
        if (! heapOfSegments(0).active) {
          heapOfSegments.remove(0)
        }
        HeapSort.heapify[SortedPartialSeqConsumer[T]](heapOfSegments, 0, heapOfSegments.size, reverseCompareSubSeq)
        result
      } else {
        None
      }
    }
  }

  def fsortParallel[T](unsorted: immutable.IndexedSeq[T], compare: Ordering[T], metric: Function1[T, Long],
                       nSegments: Int, executionContext: ExecutionContext)
                      (implicit classTag: ClassTag[T]): immutable.IndexedSeq[T] = {
    val unsortedMutable : mutable.IndexedSeq[T] = unsorted.to[mutable.IndexedSeq]
    fsortParallelM(unsortedMutable, compare, metric, nSegments, executionContext)(classTag)
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
  def fsortParallelM[T](unsorted: mutable.IndexedSeq[T], compare: Ordering[T], metric: Function1[T, Long],
                        nSegments: Int, executionContext: ExecutionContext)
                       (implicit classTag: ClassTag[T]): immutable.IndexedSeq[T] = {

    implicit val executionContextImpl = executionContext
    if (nSegments <= 0) {
      throw new IllegalArgumentException("nSegments=" + nSegments + " needs to be greater than 0")
    }
    if (nSegments == 1) {
      return FlashSort.fsort(unsorted, compare, metric)(classTag).toIndexedSeq
    }
    val nElements: Int = unsorted.size
    val step: Int = (nElements + nSegments - 1) / nSegments
    if (step <= 2) {
      return FlashSort.fsort(unsorted, compare, metric)(classTag).toIndexedSeq
    }
    val segmentBoundaries : Seq[Int] = (0 to nSegments)
      .map(i => Math.min(i * step, nElements))
    val segRange: Seq[Int] = (0 until nSegments).toIndexedSeq
    val futures: Seq[Future[Unit]] = segRange.map(
      seqIdx => Future[Unit]({
        FlashSort.fsortPartial(unsorted, segmentBoundaries(seqIdx), segmentBoundaries(seqIdx+1), compare, metric)
      })(executionContext))
    // barrier: wait for all futures to complete
    val aggr = Future.sequence(futures)
    Await.result(aggr, Duration.Inf)

    val heapOfPartialSeqs = new HeapOfSortedPartialSeqs[T](unsorted, compare, nSegments, segmentBoundaries)
    val result = (0 until nElements).flatMap(i=> heapOfPartialSeqs.read)
    result
  }

  /** for non-fsort (currently only hsort supported) */
  def hsort[T](unsorted: IndexedSeq[T], compare: Ordering[T], metric: Function1[T, Long])(implicit classTag: ClassTag[T]): IndexedSeq[T] = {
    fsortWithFactor(unsorted, compare, metric, 0.42)(classTag)
  }
}
