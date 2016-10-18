package logic

import fastparse.all._

import scala.util.parsing.combinator.JavaTokenParsers

case class FormulaPrintStyle(
                              trueSymbol: String, falseSymbol: String,
                              negation: String,
                              conjunction: String, disjunction: String,
                              implication: String, iff: String
                              ) {
}

object FormulaPrintStyle{
  val textBook = FormulaPrintStyle(
    trueSymbol = "⊤", falseSymbol = "⊥",
    negation = "¬",
    conjunction = "∧", disjunction = "∨",
    implication = "→", iff = "⇔"
  )

  val programming = FormulaPrintStyle(
    trueSymbol = "T", falseSymbol = "F",
    negation = "!",
    conjunction = "&", disjunction = "|",
    implication = "->", iff = "<=>"
  )
}

class FormulaParser(style: FormulaPrintStyle) extends JavaTokenParsers{
  def pVar = ident.map{ x => FVar(Symbol(x))}
  def pTrue = literal(style.trueSymbol).map{_ => FT}
  def pFalse = literal(style.falseSymbol).map{_ => FF}
  def pConst = pTrue | pFalse
  def pAtom = pConst | pVar
  def pValue = pAtom | "(" ~> pFormula <~ ")"
  def pNegation: Parser[Negation] = style.negation ~> (pNegation | pValue) ^^ Negation

  def layer1 = pNegation | pValue

  def pBinary(op: String, operand: Parser[Formula], create: FBinary.Creator, associateLeft: Boolean = true) =
    rep1sep(operand, op).map(terms =>{
      ( if(associateLeft) terms.tail.foldLeft(terms.head) _
        else terms.init.foldRight(terms.last) _){case (t1,t2) => create(t1,t2)}
    })

  def pAnd = pBinary(style.conjunction, layer1, And.apply)
  def pOr = pBinary(style.disjunction, pAnd, Or.apply)
  def pImply = pBinary(style.implication, pOr, Imply.apply, associateLeft = false)
  def pIff = pBinary(style.iff, pImply, Iff.apply)

  def pFormula: Parser[Formula] = pIff

  def apply(in: CharSequence) = parseAll(pFormula, in) match {
    case Success(r, _) => r
    case other => throw new Exception(other.toString)
  }
}
