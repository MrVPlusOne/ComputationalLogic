package tests

import logic.{FBinary, Formula, Negation}
import org.scalacheck.{Prop, Shrink}
import org.scalacheck.Test.Parameters
import org.scalatest.WordSpec
import org.scalatest.prop.Checkers._


/**
  * Created by weijiayi on 17/10/2016.
  */
trait FormulaTestSuit extends WordSpec {
  import RandomFormula.formulaGen

  def checkProp(prop: Prop, maxSize: Int = 80): Unit = {
    check(prop, Parameters.default.withMaxSize(maxSize).withMinSize(1).withMinSuccessfulTests(10))
  }

  def forAllFormulae(f: Formula => Boolean, maxSize: Int = 60): Unit = {
    checkProp(Prop.forAll(formulaGen)(f), maxSize = maxSize)
  }

  implicit def shrinkFormula: Shrink[Formula] = Shrink[Formula]{
    case bf: FBinary => Stream(bf.f1, bf.f2)
    case Negation(x) => Stream(x)
    case _ => Stream()
  }
}
