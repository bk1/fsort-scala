package net.itsky.sortsearch.fsort

/** currently not used. Later for a richer interface */
trait Metrizable[T] extends Ordering[T] {

  def metric : Long;
}