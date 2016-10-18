package logic


sealed trait Formula {

  override def toString: String = prettyPrint(FormulaPrintStyle.textBook)

  def combiningStrength: Int

  def prettyPrint(style: FormulaPrintStyle): String = {
    this match {
      case c: FConst => c.name(style)
      case v: FVar => v.name
      case n@Negation(x) =>
        val needWrap = x.combiningStrength < combiningStrength
        s"${style.negation}${wrap(x.prettyPrint(style), needWrap)}"
      case b: FBinary =>
        val needWrapLeft = b.f1.combiningStrength < combiningStrength || (b.f1.combiningStrength == combiningStrength && !b.combineLeft)
        val needWrapRight = b.f2.combiningStrength < combiningStrength || (b.f2.combiningStrength == combiningStrength && b.combineLeft)
        s"${wrap(b.f1.prettyPrint(style), needWrapLeft)} ${b.operator(style)} ${wrap(b.f2.prettyPrint(style), needWrapRight)}"
    }
  }

  def wrap(s: String, needWrap: Boolean): String = {
    if(needWrap) s"($s)" else s
  }

  def variables: Set[FVar]

  def unary_! : Negation = Negation(this)

  def &(f1: Formula) : And = And(this, f1)

  def |(f1: Formula) : Or = Or(this, f1)

  def ->(f1: Formula): Imply = Imply(this, f1)

  def <->(f1: Formula): Iff = Iff(this, f1)

  def substitute(varMap: Map[FVar, FConst]): Formula = this match {
    case c: FConst => c
    case v: FVar => varMap.getOrElse(v, v)
    case Negation(f) => Negation(f.substitute(varMap))
    case b: FBinary => b.create(b.f1.substitute(varMap), b.f2.substitute(varMap))
  }

  def substitute(varMap: (FVar, FConst)*): Formula = substitute(varMap.toMap)

  def evaluate: Formula

  def subFormulae: IndexedSeq[Formula] = this match {
    case a: Atom => IndexedSeq()
    case Negation(f) => f.subFormulae :+ this
    case b: FBinary => b.f1.subFormulae ++ b.f2.subFormulae :+ this
  }

  def subFormulaeNoRepeat = {
    def clean(fs: IndexedSeq[Formula]) = {
      var set = Set[Formula]()
      fs.filter(x =>{
        if(!(set contains x)){
          set += x
          true
        } else false
      })
    }
    clean(subFormulae)
  }

}

sealed trait Atom extends Formula{
  override def evaluate: Formula = this

}

sealed trait FConst extends Atom{
  def neg: FConst

  def name(style: FormulaPrintStyle): String

  override def variables: Set[FVar] = Set()

  def bool = this == FT
}

object FConst{
  def apply(b: Boolean): FConst = {
    if(b) FT else FF
  }
}

case object FT extends FConst{
  override def neg: FConst = FF

  override def name(style: FormulaPrintStyle): String = style.trueSymbol

  override implicit def combiningStrength: Int = 9
}

case object FF extends FConst{
  override def neg: FConst = FT

  override def name(style: FormulaPrintStyle): String = style.falseSymbol

  override implicit def combiningStrength: Int = 9
}

case class FVar(s: Symbol) extends Atom{
  def name = s.name

  override def combiningStrength: Int = 9

  override def variables: Set[FVar] = Set(this)
}

case class Negation(f: Formula) extends Formula{
  def operator = IndexedSeq("!","¬")

  override implicit def combiningStrength: Int = 8

  override def variables: Set[FVar] = f.variables

  override def evaluate: Formula = f.evaluate match {
    case c: FConst => c.neg
    case other => Negation(other)
  }
}

sealed trait FBinary extends Formula{
  def f1: Formula
  def f2: Formula

  def operator(style: FormulaPrintStyle): String

  def create: (Formula,Formula) => FBinary

  def combineLeft = true

  override def variables: Set[FVar] = f1.variables ++ f2.variables

  override def evaluate: Formula = calculate(f1.evaluate, f2.evaluate)

  // with arguments already evaluated
  protected def calculate(c1: Formula, c2: Formula): Formula
}

object FBinary{
  type Creator = (Formula, Formula) => FBinary
}


case class And(f1: Formula, f2: Formula) extends FBinary{

  override def operator(style: FormulaPrintStyle): String = style.conjunction

  override def create: (Formula, Formula) => FBinary = And.apply

  override implicit def combiningStrength: Int = 7

  // with arguments already evaluated
  override protected def calculate(c1: Formula, c2: Formula): Formula = (c1, c2) match {
    case (FT, x) => x
    case (x, FT) => x
    case (FF, _) => FF
    case (_, FF) => FF
    case _ => create(c1,c2)
  }
}

case class Or(f1: Formula, f2: Formula) extends FBinary{

  override def operator(style: FormulaPrintStyle): String = style.disjunction

  override def create: (Formula, Formula) => FBinary = Or.apply

  override implicit def combiningStrength: Int = 6

  // with arguments already evaluated
  override protected def calculate(c1: Formula, c2: Formula): Formula = {
    if(c1 == FT || c2 == FT) FT
    else if(c1 == FF) c2
    else if(c2 == FF) c1
    else create(c1,c2)
  }
}

case class Imply(ante: Formula, consequence: Formula) extends FBinary{
  override def f1 = ante
  override def f2 = consequence

  override def combineLeft: Boolean = false

  override def create: (Formula, Formula) => FBinary = Imply.apply

  override def operator(style: FormulaPrintStyle): String = style.implication

  override implicit def combiningStrength: Int = 5

  // with arguments already evaluated
  override protected def calculate(c1: Formula, c2: Formula): Formula = (c1,c2) match {
    case (cc1: FConst, cc2: FConst) => FConst(cc1.bool && !cc2.bool).neg
    case _ => create(c1,c2)
  }

}

case class Iff(f1: Formula, f2: Formula) extends FBinary{
  override def create: (Formula, Formula) => FBinary = Iff.apply

  override def operator(style: FormulaPrintStyle): String = style.iff

  override implicit def combiningStrength: Int = 4

  // with arguments already evaluated
  override protected def calculate(c1: Formula, c2: Formula): Formula = (c1,c2) match {
    case (cc1: FConst, cc2: FConst) => FConst(cc1 == cc2)
    case _ => create(c1,c2)
  }
}


object Formula {


}
