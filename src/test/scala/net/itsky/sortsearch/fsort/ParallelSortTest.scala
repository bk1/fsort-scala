
package net.itsky.sortsearch.fsort

import org.scalatest._

import scala.collection.mutable.IndexedSeq

class FlashSortTest extends FlatSpec with Matchers {

  val cmp = Ordering.Int
  val metric = (x : Int) => x.longValue() << 8

  val cmps = Ordering.String
  val metrics = (x : String) => {
    x.length match {
      case 0 => 0L
      case 1 => (x.charAt(0) + 1L)* 65537* 65537* 16385
      case 2 => ((x.charAt(0) + 1L) * 65537 + x.charAt(1) + 1)* 65537* 16385
      case 3 => (((x.charAt(0) + 1L) * 65537 + x.charAt(1) + 1) * 65537 + x.charAt(2) + 1) * 16385
      case _ => (((x.charAt(0) + 1L) * 65537 + x.charAt(1) + 1) * 65537 + x.charAt(2) + 1) * 16385 + (x.charAt(3) >> 2) + 1
    }
  }

  "An empty Array" should "be sorted without any changes" in {
    val unsorted : Array[Int] = Array()
    val arr = FlashSort.fsort(unsorted, cmp, metric)
    arr.length should be (0)
  }

  "An empty Array" should "be sorted in place without any changes" in {
    val unsorted : Array[Int] = Array()
    val arr = FlashSort.fsortPartial(unsorted, 0, 0, cmp, metric)
    arr.length should be (0)
    arr should be (unsorted)
  }


  "An empty segment of an Array" should "be sorted in place without any changes" in {
    val unsorted : Array[Int] = Array(79, 78, 77, 76)
    val arr = FlashSort.fsortPartial(unsorted, 2, 2, cmp, metric)
    arr.length should be (4)
    arr should be (unsorted)
    arr(0) should be (79)
    arr(1) should be (78)
    arr(2) should be (77)
    arr(3) should be (76)
  }

  "An one element Array" should "be sorted without any changes" in {
    val unsorted : Array[Int] = Array(77)
    val arr = FlashSort.fsort(unsorted, cmp, metric)
    arr.length should be (1)
    arr(0) should be (77)
  }

  "An one element Array" should "be sorted in place without any changes" in {
    val unsorted : Array[Int] = Array(77)
    val arr = FlashSort.fsortPartial(unsorted, 0, 1, cmp, metric)
    arr.length should be (1)
    arr(0) should be (77)
  }

  "An one element segment Array" should "be sorted in place without any changes" in {
    val unsorted : Array[Int] = Array(79, 78, 77, 76, 75)
    val arr = FlashSort.fsortPartial(unsorted, 2, 3, cmp, metric)
    arr.length should be (5)
    arr should be (unsorted)
    arr(0) should be (79)
    arr(1) should be (78)
    arr(2) should be (77)
    arr(3) should be (76)
    arr(4) should be (75)
  }

  "An two element Array duplicating one element" should "be sorted without any changes" in {
    val unsorted : Array[Int] = Array(79, 79)
    val arr = FlashSort.fsort(unsorted, cmp, metric)
    arr.length should be (2)
    arr(0) should be (79)
    arr(1) should be (79)
  }

  "An two element Array segement duplicating one element" should "be sorted without any changes" in {
    val unsorted : Array[Int] = Array(99, 79, 79, 11)
    val arr = FlashSort.fsortPartial(unsorted, 1, 3, cmp, metric)
    arr.length should be (4)
    arr(0) should be (99)
    arr(1) should be (79)
    arr(2) should be (79)
    arr(3) should be (11)
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

  "An ten element Array" should "be sorted in place ascendingly" in {
    val arrs : Seq[Array[Int]] = Seq(Array(10, 8, 9, 4, 5, 6, 1, 2, 3, 7), Array(10, 9, 8, 7, 6, 5, 4, 3, 2, 1))
    for (unsorted <- arrs) {
      val arr = FlashSort.fsortPartial(unsorted, 0, 10, cmp, metric)
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

  "An ten element segment of an Array" should "be sorted ascendingly" in {
    val arrs : Seq[Array[Int]] = Seq(Array(909, 808, 10, 8, 9, 4, 5, 6, 1, 2, 3, 7, 707, 606), Array(909, 808, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 707, 606))
    for (unsorted <- arrs) {
      val arr = FlashSort.fsortPartial(unsorted, 2, 12, cmp, metric)
      arr.length should be (14)
      arr(0) should be (909)
      arr(1) should be (808)
      arr(2) should be (1)
      arr(3) should be (2)
      arr(4) should be (3)
      arr(5) should be (4)
      arr(6) should be (5)
      arr(7) should be (6)
      arr(8) should be (7)
      arr(9) should be (8)
      arr(10) should be (9)
      arr(11) should be (10)
      arr(12) should be (707)
      arr(13) should be (606)
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

  "An ten element Array segment with duplicates" should "be sorted ascendingly" in {
    val arrs : Seq[Array[Int]] = Seq(Array(99, 98, 10, 8, 8, 4, 4, 6, 0, 2, 2, 6, 97,96), Array(99, 98, 10, 8, 8, 6, 6, 4, 4, 2, 2, 0, 97, 96))
    for (unsorted <- arrs) {
      val arr = FlashSort.fsortPartial(unsorted, 2, 12, cmp, metric)
      arr.length should be (14)
      arr(0) should be (99)
      arr(1) should be (98)
      arr(2) should be (0)
      arr(3) should be (2)
      arr(4) should be (2)
      arr(5) should be (4)
      arr(6) should be (4)
      arr(7) should be (6)
      arr(8) should be (6)
      arr(9) should be (8)
      arr(10) should be (8)
      arr(11) should be (10)
      arr(12) should be (97)
      arr(13) should be (96)
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

  "An thousand element Array" should "be sorted in place ascendingly" in {
    val unsorted = (1000 to 1 by -1).to[Array]
    unsorted.length should be (1000)
    unsorted(0) should be (1000)
    unsorted(999) should be (1)
    val arr = FlashSort.fsortPartial(unsorted, 0,1000, cmp, metric)
    arr.length should be (1000)
    for (idx <- 0 to 999) {
      arr(idx) should be (idx + 1)
    }
  }
  "An hundred element Array of Strings" should "be be sorted ascendingly" in {
    val unsortedMaster = Array("Hetf8mnx",
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
    unsortedMaster.length should be(100)
    val arrH = unsortedMaster.clone
    val unsortedF : IndexedSeq[String] = unsortedMaster.clone.to[IndexedSeq]
    val arrP : IndexedSeq[String] = unsortedMaster.clone.to[IndexedSeq]
    HeapSort.hsort(arrH, cmps)
    FlashSort.fsortPartial[String](arrP, 0, 100, cmps, metrics)
    val arrF  : IndexedSeq[String] =  FlashSort.fsort[String](unsortedF, cmps, metrics)
    arrH.length should be(100)
    arrP.length should be(100)
    arrF.length should be(100)
    arrP should be (arrH)
    arrF should be (arrP)
    for (idx <- 0 until 100) {
      arrP(idx) should be (arrH(idx))
      arrF(idx) should be (arrP(idx))
    }
    for (idx <- 1 until 100) {
      arrH(idx).compareTo(arrH(idx-1)) should be >= 0
      metrics(arrH(idx)) should be >= metrics(arrH(idx-1))
      metrics(arrP(idx)) should be >= metrics(arrP(idx-1))
      metrics(arrF(idx)) should be >= metrics(arrF(idx-1))
      arrP(idx).compareTo(arrP(idx-1)) should be >= 0
      arrF(idx).compareTo(arrF(idx-1)) should be >= 0
    }
  }
}
