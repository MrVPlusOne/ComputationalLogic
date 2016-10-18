package logic

/**
  * Created by weijiayi on 16/10/2016.
  */
object Test {
  def main(args: Array[String]): Unit = {
    import logic._

    val example = "P∧Q → P → Q"

//    val p = new FormulaParser(FormulaPrintStyle.textBook)
//
//    p.test.parse("(x)")
    FormulaParser.test.parse("(hi)")
  }
}
