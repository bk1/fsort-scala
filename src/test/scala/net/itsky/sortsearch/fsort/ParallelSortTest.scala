
package net.itsky.sortsearch.fsort

import org.scalatest._

import scala.collection.{immutable, mutable}
import scala.concurrent.ExecutionContext

class ParallelSortTest extends FlatSpec with Matchers {

  val cmp = Ordering.Int
  val metric = (x: Int) => x.longValue() << 8

  val cmps = Ordering.String
  val metrics = (x: String) => {
    x.length match {
      case 0 => 0L
      case 1 => (x.charAt(0) + 1L) * 65537 * 65537 * 16385
      case 2 => ((x.charAt(0) + 1L) * 65537 + x.charAt(1) + 1) * 65537 * 16385
      case 3 => (((x.charAt(0) + 1L) * 65537 + x.charAt(1) + 1) * 65537 + x.charAt(2) + 1) * 16385
      case _ => (((x.charAt(0) + 1L) * 65537 + x.charAt(1) + 1) * 65537 + x.charAt(2) + 1) * 16385 + (x.charAt(3) >> 2) + 1
    }
  }

  val ec: ExecutionContext = ExecutionContext.global

  "An empty Array" should "be sorted without any changes" in {
    for (i <- (1 until 10)) {
      val unsorted: Array[Int] = Array()
      val arr = ParallelSort.fsortParallelM(unsorted, cmp, metric, i, ec)
      arr.length should be(0)
    }
  }


  "An one element Array" should "be sorted without any changes" in {
    for (i <- (1 until 10)) {
      val unsorted: Array[Int] = Array(77)
      val arr = ParallelSort.fsortParallelM(unsorted, cmp, metric, i, ec)
      arr.length should be(1)
      arr(0) should be(77)
    }
  }

  "An two element Array duplicating one element" should "be sorted without any changes" in {
    for (i <- (1 until 10)) {
      val unsorted: Array[Int] = Array(79, 79)
      val arr = ParallelSort.fsortParallelM(unsorted, cmp, metric, i, ec)
      arr.length should be(2)
      arr(0) should be(79)
      arr(1) should be(79)
    }
  }

  "An two element ascending Array" should "be sorted without any changes" in {
    for (i <- (1 until 10)) {
      val unsorted: Array[Int] = Array(11, 12)
      val arr = ParallelSort.fsortParallelM(unsorted, cmp, metric, i, ec)
      arr.length should be(2)
      arr(0) should be(11)
      arr(1) should be(12)
    }
  }

  "An two element descending Array" should "be sorted ascendingly" in {
    for (i <- (1 until 10)) {
      val unsorted: Array[Int] = Array(12, 11)
      val arr = ParallelSort.fsortParallelM(unsorted, cmp, metric, i, ec)
      arr.length should be(2)
      arr(0) should be(11)
      arr(1) should be(12)
    }
  }

  "An three element Array" should "be sorted ascendingly" in {
    for (i <- (1 until 10)) {
      val arrs: Seq[Array[Int]] = Seq(Array(1, 2, 3), Array(2, 1, 3), Array(1, 3, 2), Array(2, 3, 1), Array(3, 2, 1), Array(3, 1, 2))
      for (unsorted <- arrs) {
        val arr = ParallelSort.fsortParallelM(unsorted, cmp, metric, i, ec)
        arr.length should be(3)
        arr(0) should be(1)
        arr(1) should be(2)
        arr(2) should be(3)
      }
    }
  }

  "An four element Array" should "be sorted ascendingly" in {
    for (i <- (1 until 10)) {
      val arrs: Seq[Array[Int]] = Seq(Array(1, 2, 3, 4), Array(2, 1, 4, 3), Array(1, 3, 4, 2), Array(2, 3, 4, 1), Array(4, 3, 2, 1), Array(3, 4, 1, 2))
      for (unsorted <- arrs) {
        val arr = ParallelSort.fsortParallelM(unsorted, cmp, metric, i, ec)
        arr.length should be(4)
        arr(0) should be(1)
        arr(1) should be(2)
        arr(2) should be(3)
        arr(3) should be(4)
      }
    }
  }

  "An five element Array" should "be sorted ascendingly" in {
    for (i <- (1 until 10)) {
      val arrs: Seq[Array[Int]] = Seq(Array(1, 2, 3, 4, 5), Array(5, 4, 3, 2, 1), Array(1, 5, 2, 4, 3))
      for (unsorted <- arrs) {
        val arr = ParallelSort.fsortParallelM(unsorted, cmp, metric, i, ec)
        //println("i=" + i + " unsorted=" +unsorted.toList + " arr=" + arr)
        arr.length should be(5)
        //println("i=" + i + " arr=" + arr.toList)
        arr(0) should be(1)
        arr(1) should be(2)
        arr(2) should be(3)
        arr(3) should be(4)
        arr(4) should be(5)
      }
    }
  }

  "An six element Array" should "be sorted ascendingly" in {
    for (i <- (1 until 10)) {
      val arrs: Seq[Array[Int]] = Seq(Array(1, 2, 3, 4, 5, 6), Array(6, 5, 4, 3, 2, 1), Array(1, 5, 2, 6, 4, 3))
      for (unsorted <- arrs) {
        val arr = ParallelSort.fsortParallelM(unsorted, cmp, metric, i, ec)
        //println("i=" + i + " unsorted=" +unsorted + " arr=" + arr)
        arr.length should be(6)
        arr(0) should be(1)
        arr(1) should be(2)
        arr(2) should be(3)
        arr(3) should be(4)
        arr(4) should be(5)
        arr(5) should be(6)
      }
    }
  }

  "An ten element Array" should "be sorted ascendingly" in {
    for (i <- (1 until 20)) {
      val arrs: Seq[Array[Int]] = Seq(Array(10, 8, 9, 4, 5, 6, 1, 2, 3, 7), Array(10, 9, 8, 7, 6, 5, 4, 3, 2, 1))
      for (unsorted <- arrs) {
        //println("i=" + i + " unsorted=" +unsorted)
        val arr = ParallelSort.fsortParallelM(unsorted, cmp, metric, i, ec)
        // println("i=" + i + " unsorted=" +unsorted + " arr=" + arr)
        arr.length should be(10)
        arr(0) should be(1)
        arr(1) should be(2)
        arr(2) should be(3)
        arr(3) should be(4)
        arr(4) should be(5)
        arr(5) should be(6)
        arr(6) should be(7)
        arr(7) should be(8)
        arr(8) should be(9)
        arr(9) should be(10)
      }
    }
  }

  "An 14 element Array" should "be sorted ascendingly" in {
    for (i <- (1 until 20)) {
      val arrs: Seq[Array[Int]] = Seq(Array(909, 808, 10, 8, 9, 4, 5, 6, 1, 2, 3, 7, 707, 606), Array(909, 808, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 707, 606))
      for (unsorted <- arrs) {
        val fsorted = FlashSort.fsort(unsorted.clone, cmp, metric)
        val parsorted = ParallelSort.fsortParallelM(unsorted.clone, cmp, metric, i, ec)
        parsorted.length should be(fsorted.length)
        for (idx <- (0 until fsorted.length)) {
          if (parsorted(idx) != fsorted(idx)) {
            println("idx=" + idx + " i=" + i)
          }
          parsorted(idx) should be(fsorted(idx))
        }
      }
    }
  }

  "An ten element Array with duplicates" should "be sorted ascendingly" in {
    for (i <- (1 until 20)) {
      val arrs: Seq[Array[Int]] = Seq(Array(10, 8, 8, 4, 4, 6, 0, 2, 2, 6), Array(10, 8, 8, 6, 6, 4, 4, 2, 2, 0))
      for (unsorted <- arrs) {
        val fsorted = FlashSort.fsort(unsorted.clone, cmp, metric)
        val parsorted = ParallelSort.fsortParallelM(unsorted.clone, cmp, metric, i, ec)
        parsorted.length should be(fsorted.length)
        for (idx <- (0 until fsorted.length)) {
          if (parsorted(idx) != fsorted(idx)) {
            println("idx=" + idx + " i=" + i)
          }
          parsorted(idx) should be(fsorted(idx))
        }
        parsorted.length should be(10)
        parsorted(0) should be(0)
        parsorted(1) should be(2)
        parsorted(2) should be(2)
        parsorted(3) should be(4)
        parsorted(4) should be(4)
        parsorted(5) should be(6)
        parsorted(6) should be(6)
        parsorted(7) should be(8)
        parsorted(8) should be(8)
        parsorted(9) should be(10)
      }
    }
  }

  "An thousand element Array" should "be sorted ascendingly" in {
    for (i <- (1 until 20)) {
      val unsorted = (1000 to 1 by -1).to[Array]
      unsorted.length should be(1000)
      unsorted(0) should be(1000)
      unsorted(999) should be(1)
      val parsorted = ParallelSort.fsortParallelM(unsorted, cmp, metric, i, ec)
      parsorted.length should be(1000)
      for (idx <- 0 to 999) {
        if (parsorted(idx) != idx + 1) {
          println("idx=" + idx + " i=" + i)
        }
        parsorted(idx) should be(idx + 1)
      }
    }
  }

  "An hundred element Array of Strings" should "be be sorted ascendingly" in {

    for (i : Int <- (1 until 200)) {
      checkParallelStringSort(i)
    }
  }

  private def checkParallelStringSort(i: Int) = {
    val unsortedMaster = mutable.IndexedSeq("Hetf8mnx",
      "6spax",
      "jloadknys",
      ")ne-ar",
      "Lsmidiff",
      "nxpmtoppm",
      "nroff2tnxt",
      "_ppmtotnrm",
      "aguatclnau-6",
      "otty",
      "Wppmtosixnl",
      ";appstrnamcli",
      "+iutnl_lid",
      "Grpdfcrop",
      "1guatfiud-6",
      "[tnxlivnoufly",
      "Cpidstat",
      "Fdb48_priutlog",
      ":pumpsur",
      "!ldaperl",
      ")sg_logs",
      ":siguou-oaeth2plegiu-tnsts",
      "wcolliuk",
      "Ppumtiln",
      "ornuamn",
      "5iostat",
      "\'chnck_fornusic",
      "kpsicc",
      "kktnluntsnrvicn",
      "(moesntwnaks",
      "uuvidia-beg-rnport.sh",
      "Apic",
      "\"digikam",
      "KsudApplnSiugln",
      "`makndb",
      "ggtk-nucodn-symbolic-svg",
      "3nmacs",
      "Xblenmoou",
      "?showcousolnfout",
      "f411toppm",
      "Zpto_lnusstack",
      "Diutnl_aedio_demp",
      "urpcgnu",
      "lpknxnc",
      ".dirmugr-clinut",
      "mjscal",
      "Zdf",
      "Elea",
      "Gppmtopict",
      "0kuntattach",
      "Ugcc-raulib-6",
      "^tnxmfstart",
      "upampaiutspill",
      "Ysmbclinut",
      "Zhtdb_load",
      "*lea5.2",
      ":kaccnss",
      "Xscoet",
      "Yse",
      "lps2ascii",
      "Jxdg-nmail",
      "akicoufiudnr",
      "]procmail",
      "Jldappasswd",
      "Ygndit",
      "]reby",
      "Preucou",
      "Qppmhist",
      "vpacmd",
      "nmkuod",
      "Tpkactiou",
      "*tnstrb",
      "Zpbmlifn",
      "oobjcopy",
      "viutnl_bios_rnadnr",
      "|rnsolvnip",
      "@ameFormat.sh",
      "Oicouv",
      "dpatch-mntamail",
      "dpamsemmcol",
      "Wsafn-rm",
      ",acyclic",
      "/tnlunt",
      ">dotty",
      "",
      "x",
      "uv",
      "!pg_tnst_timiug",
      "Tgit-rncnivn-pack",
      "3akouadisnlftnst",
      "+swig",
      ";iustallchnckmmozroots",
      "?tnxcoeut",
      "qlcouvnrt-qt5wpgmuorm",
      "|hpftodit",
      "kgd2topug",
      "@BackGroeud",
      "@mnrgn-pciids",
      "(dbiproxy",
      "?libyei-tnrmiual")

    unsortedMaster.size should be(100)
    val sortableHeapSort = unsortedMaster.clone.toArray
    val sortedParallel: immutable.IndexedSeq[String] = ParallelSort.fsortParallel(unsortedMaster.toIndexedSeq, cmps, metrics, i, ec)
    HeapSort.hsort(sortableHeapSort, cmps)
    sortableHeapSort.length should be(100)
    sortedParallel.length should be(100)
    for (idx <- 0 until 100) {
      if (sortedParallel(idx) != sortableHeapSort(idx)) {
        println("idx=" + idx + " i=" + i)
      }
      sortedParallel(idx) should be (sortableHeapSort(idx))
    }
    for (idx <- 1 until 100) {
      sortableHeapSort(idx).compareTo(sortableHeapSort(idx - 1)) should be >= 0
      metrics(sortableHeapSort(idx)) should be >= metrics(sortableHeapSort(idx - 1))
      metrics(sortedParallel(idx)) should be >= metrics(sortedParallel(idx - 1))
      sortedParallel(idx).compareTo(sortedParallel(idx - 1)) should be >= 0
    }
  }
}
