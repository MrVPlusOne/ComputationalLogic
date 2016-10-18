package logic

/**
  * the truth table of a formula
  */
case class TruthTable(header: IndexedSeq[Formula], rows: IndexedSeq[IndexedSeq[FConst]]){
  def printTable(sep: Int = 3)(implicit style: FormulaPrintStyle) {
    val headers = header.map(_.prettyPrint(style))
    def printARow(row: IndexedSeq[Formula]): Unit ={
      headers.indices.foreach(i =>{
        print(" "*sep)
        val elem = row(i).prettyPrint(style)
        print(elem)
        print(" "*(headers(i).length - elem.length))
      })
      print("\n")
    }
    printARow(header)
    rows.foreach(printARow)
  }
}

object TruthTable {
  def create(f: Formula): TruthTable = {

    val variables = f.variables.toIndexedSeq
    val formulas = f.subFormulaeNoRepeat
    val vn = variables.size
    val rows = IntegerMath.power(2, vn)
    val tableRows =  for(r <- -0 until rows) yield {
      val varValues = IntegerMath.baseNRepresentation(r, base = 2, bits = vn).map(bit => {
        if(bit == 0) FF else FT
      }).toIndexedSeq
      val varMap = variables.zip(varValues).toMap
      val formulaValues = formulas.map(_.substitute(varMap).evaluate.asInstanceOf[FConst])
      varValues ++ formulaValues
    }
    TruthTable(variables ++ formulas, tableRows)
  }

  def values(f: Formula, variables: Set[FVar]): IndexedSeq[Boolean] = {
    val vn = variables.size
    val rows = IntegerMath.power(2, vn)
    (0 until rows).map{ r =>
      val varValues = IntegerMath.baseNRepresentation(r, base = 2, bits = vn).map(bit => {
        if(bit == 0) FF else FT
      }).toIndexedSeq
      val varMap = variables.zip(varValues).toMap
      f.substitute(varMap).evaluate == FT
    }
  }

  def validity(f: Formula): Boolean = values(f, f.variables).forall(x => x)

}
