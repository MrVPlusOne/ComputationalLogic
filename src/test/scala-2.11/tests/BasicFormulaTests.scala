package tests

import logic._
import logic.NormalForm._

/**
  * Created by weijiayi on 17/10/2016.
  */
class BasicFormulaTests extends FormulaTestSuit {

  "A formula" should {
    "equal to itself" in {
      forAllFormulae(f => f == f)
    }
  }

  "substitution" should {
    "remain the same when apply to empty" in {

      forAllFormulae(f => f.substitute() == f)

    }
  }

  "evaluation" should {
    "not change truth table" in {

      forAllFormulae(f =>
        TruthTable.values(f.evaluate, f.variables) == TruthTable.values(f, f.variables),
        maxSize = 50
      )

    }
  }

  "nnf" should {
    "not contain complex negation" in {
      forAllFormulae(f => f.nnf.subFormulaeNoRepeat.forall{
        case Negation(f: FBinary) => false
        case Negation(Negation(_)) => false
        case _ => true
      })
    }

    "not change truth table" in {
      forAllFormulae(f => {
        val nnf = f.nnf
        TruthTable.values(f,f.variables) == TruthTable.values(nnf, f.variables)
      }, maxSize = 50)
    }
  }

  "parser" should {
    "restore printed result" in {
      val style1 = FormulaPrintStyle.textBook
//      val parser = new FormulaParser(style1)
//      forAllFormulae(f => {
//        parser(f.prettyPrint(style1)) == f
//      })
    }
  }
}
