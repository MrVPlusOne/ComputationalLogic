package tests

import logic._
import org.scalacheck.Gen

/**
  * Formula Random Generators
  */
object RandomFormula {
  def varGen(range: Int) = {
    Gen.choose(0, range).map(i => FVar(Symbol(s"a$i")))
  }

  val constGen = Gen.oneOf(FT, FF)

  def atomGen(varRange: Int): Gen[Formula] = {
    Gen.oneOf(1,2,3) flatMap {
      case 1 => varGen(varRange)
      case 2 => constGen
      case 3 => atomGen(varRange).map(Negation)
    }
  }

  def sizedFormula(size: Int, varRange: Int): Gen[Formula] = {
    if (size < 2) atomGen(varRange)
    else {
      for {
        lSize <- Gen.choose(0, size)
        left <- sizedFormula(lSize, varRange)
        right <- sizedFormula(size - lSize, varRange)
        bi <- Gen.oneOf[FBinary.Creator](And.apply _, Or.apply _, Imply.apply _, Iff.apply _)
      } yield bi.apply(left, right)
    }
  }

  val formulaGen = Gen.sized(s => sizedFormula(s, varRange = 6))
}