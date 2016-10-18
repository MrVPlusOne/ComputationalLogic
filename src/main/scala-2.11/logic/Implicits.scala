package logic

import scala.language.implicitConversions

/**
  * Created by weijiayi on 16/10/2016.
  */
object Implicits {
  implicit def fVarFromSymbol(s: Symbol): FVar = FVar(s)

}
