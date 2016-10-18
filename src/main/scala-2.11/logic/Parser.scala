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

class NewParser(style: FormulaPrintStyle) extends JavaTokenParsers{
  def pVar = ident.map{ x => FVar(Symbol(x))}
  def pTrue = literal(style.trueSymbol).map{_ => FT}
  def pFalse = literal(style.falseSymbol).map{_ => FF}
  def pConst = pTrue | pFalse
  def pAtom = pConst | pVar
  def pValue = pAtom | "(" ~> pFormula <~ ")"
  def pNegation = style.negation ~> pValue ^^ Negation

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

  def apply(in: CharSequence) = parseAll(pFormula, in)
}

/**
  * Parsing formulas
  */
object FormulaParser {
  val style = FormulaPrintStyle.programming

  val spaceChar = CharIn(" \n\t")
  val spaces = spaceChar.rep
  val varNameChar = CharIn('0' to '9','a' to 'z', 'A' to 'Z')

  val parseTrue = P(style.trueSymbol).map(_ => FT)
  val parseFalse = P(style.falseSymbol).map(_ => FF)
  val parseConst = parseTrue | parseFalse
  val parseVar = P(varNameChar.rep(min=1).!).map(name => FVar(Symbol(name)))
  val parseAtom = parseConst | parseVar

//  val parseNegate: Parser[Negation] = P(style.negation~spaces~(parseAtom | parseLayer1)).map(Negation)
  val parseLayer1 =  parseAtom | parseParentheses


  def binaryParser(op: String, layer: Parser[Formula], create: FBinary.Creator, associateLeft: Boolean = true) =
    P(layer.rep(min = 1, sep = spaces ~ op ~ spaces)).map{ terms =>
      if(associateLeft) terms.tail.foldLeft(terms.head){ case (t1,t2) => create(t1,t2)}
      else terms.init.foldRight(terms.last){ case (t1,t2) => create(t1,t2)}
    }

  val parseAnd = binaryParser(style.conjunction, parseLayer1, And.apply)

  val parseOr = binaryParser(style.disjunction, parseAnd, Or.apply)

  val parseImply = binaryParser(style.implication, parseOr, Imply.apply, associateLeft = false)

  val parseIff = binaryParser(style.iff, parseImply, Iff.apply)

  // simple

  def mayInParens[A](p: => Parser[A]): Parser[A] = p | "(" ~/ spaces~mayInParens(p)~spaces~")"

  lazy val test: Parser[Atom] = parseAtom | ("(" ~/ spaces~ aux ~spaces~")")
  lazy val aux = test

  val parseParentheses: Parser[Formula] = "(" ~/ spaces~parseFormula~spaces~")"

  val parseFormula = spaces ~ parseIff ~ spaces ~ End



  def apply(expr: String) = parseFormula.parse(expr).get.value
}
