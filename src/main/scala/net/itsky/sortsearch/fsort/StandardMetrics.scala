package net.itsky.sortsearch.fsort

object StandardMetrics {

  val utf16 = (x : String) => {
    x.length match {
      case 0 => 0L
      case 1 => (x.charAt(0) + 1L) * 65537 * 65537 * 16385
      case 2 => ((x.charAt(0) + 1L) * 65537 + x.charAt(1) + 1) * 65537 * 16385
      case 3 => (((x.charAt(0) + 1L) * 65537 + x.charAt(1) + 1) * 65537 + x.charAt(2) + 1) * 16385
      case _ => (((x.charAt(0) + 1L) * 65537 + x.charAt(1) + 1) * 65537 + x.charAt(2) + 1) * 16385 + (x.charAt(3) >> 2) + 1
    }
  }

  def iso646irvC(x : String, idx : Int) : (Int, Boolean) = {
    if (idx >= x.length) {
      (0, false)
    } else {
      val c : Char = x.charAt(idx)
      if (c < 128) {
        (c + 1, true)
      } else {
        (129, false)
      }
    }
  }

  val iso646irv = (x : String) => {
    var result : Long = 0L
    var cont : Boolean = true
    for (idx <- (0 until 8)) {
      result *= 130L
      if (cont) {
        val (cm, contx) = iso646irvC(x, idx)
        cont = contx
        result += cm
      }
    }
    result
  }


  def iso88591C(x : String, idx : Int) : (Int, Boolean) = {
    if (idx >= x.length) {
      (0, false)
    } else {
      val c : Char = x.charAt(idx)
      if (c < 256) {
        (c + 1, true)
      } else {
        (257, false)
      }
    }
  }

  val iso88591 = (x : String) => {
    var result : Long = 0L
    var cont : Boolean = true
    for (idx <- (0 until 7)) {
      result *= 258
      if (cont) {
        val (cm, contx) = iso88591C(x, idx)
        cont = contx
        result += cm
      }
    }
    result
  }


  def cyrC(x : String, idx : Int) : (Int, Boolean) = {
    if (idx >= x.length) {
      (0, false)
    } else {
      val c = x.charAt(idx)
      if (c < 0x0080) {
        (c + 1, true)  // 1..0x0080
      } else if (c < 0x0400) {
        (0x0081, false)
      } else if (c < 0x0500) {
        (c - 0x0400 + 0x0082, true) // 0x0082..0x0181
      } else {
        (0x0182, false) // 386
      }
    }
  }

  val cyr = (x : String) => {
    var result : Long = 0
    var cont : Boolean = true
    for (idx <- (0 until 7)) {
      result *= 387
      if (cont) {
        val (cm, contx) = cyrC(x, idx)
        cont = contx
        result += cm
      }
    }
    result
  }
}
