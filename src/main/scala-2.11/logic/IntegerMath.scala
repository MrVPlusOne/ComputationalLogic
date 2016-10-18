package logic

/**
  * Created by weijiayi on 16/10/2016.
  */
object IntegerMath {
  def power(b: Int, p: Int): Int = {
    if(p==0) 1
    else if(p==1) b
    else {
      val half = p / 2
      val rem = p - 2 * half
      val ph = power(b,half)
      power(b,rem) * ph * ph
    }
  }

  def baseNRepresentation(n: Int, base: Int, bits: Int): Array[Int] = {
    val a = new Array[Int](bits)
    var remainder = n
    for(b <- 0 until bits){
      a(b) = remainder % base
      remainder /= base
    }
    a
  }
}