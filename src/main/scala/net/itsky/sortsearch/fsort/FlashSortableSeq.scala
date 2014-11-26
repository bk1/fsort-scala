package net.itsky.sortsearch.fsort

import scala.collection.mutable.IndexedSeq


trait FlashSortableSeq[T] extends IndexedSeq[T] {
	def fsort(compare : Function2[T, T, Int], metric : Function1[T, Long]) : Unit = {
	  // TODO implement this
	}
}