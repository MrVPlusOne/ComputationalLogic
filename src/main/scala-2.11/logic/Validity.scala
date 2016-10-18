package logic

import logic._

/**
  * Verify the validity of a formula
  */
object Validity {
//  type VarMap = Map[FVar, FConst]
//
//  def validity(f: Formula, printer: String => Unit): Stream[VarMap] = {
//    def deduce(f: Formula, satisfy: Boolean, varMap: VarMap, id: IndexedSeq[Int]): Stream[VarMap] = {
//      def info(s: String) = s"${"  " * id.length} ${id.mkString(".")}  | $s"
//
//      f match {
//        case v: FVar =>
//          val target = FConst(satisfy)
//          if(varMap.get(v).contains(target)){
//            info(s"$v should be $target, but already defined to be ${target.neg}")
//            Stream()
//          }
//          else Stream(varMap+((v, FConst(satisfy))))
//        case Negation(x) => deduce(x, satisfy = !satisfy, varMap)
//        case And(a, b) =>
//          if(satisfy)
//            for{
//              m1 <- deduce(a, true, varMap)
//              m2 <- deduce(b, true, m1)
//            } yield m2
//          else deduce(a, false, varMap) ++ deduce(b, false, varMap)
//      }
//    }
//
//    deduce(f, false, Map())
//  }


}
