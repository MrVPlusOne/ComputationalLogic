package logic

/**
  * Normal Form operations
  */
class NormalForm(f: Formula){
  import NormalForm._

  /** negation normal form */
  def nnf = negationForm(f)

  /** disjunctive normal form */
  def dnf = disjunctionForm(f)

  /** conjunction normal form */
  def cnf= conjunctiveForm(f)
}

object NormalForm {

  def negationForm(f: Formula): Formula = f match {
    case a: Atom => a
    case Negation(c: FConst) => c.neg
    case Negation(v: FVar) => f
    case Negation(Negation(x)) => negationForm(x)
    case Negation(And(f1,f2)) => negationForm(!f1) | negationForm(!f2)
    case Negation(Or(f1,f2)) => negationForm(!f1) & negationForm(!f2)
    case Negation(f1: FBinary) => negationForm(!negationForm(f1))
    case Imply(f1,f2) => negationForm(!f1) | negationForm(f2)
    case Iff(f1,f2) => negationForm(f1 -> f2) & negationForm(f2->f1)
    case And(f1, f2) => negationForm(f1) & negationForm(f2)
    case Or(f1, f2) => negationForm(f1) | negationForm(f2)
  }

  def disjunctionForm(f: Formula): Formula = {
    def rec(f: Formula): Formula = f match {
      case And(Or(f1,f2),f3) => rec(f1 & f3) | rec(f2 & f3)
      case And(f3, Or(f1,f2)) => rec(f3 & f1) | rec(f3 & f2)
      case b: FBinary =>
        val nb = b.create(rec(b.f1), rec(b.f2))
        if(nb != b) rec(nb) else nb
      case a: Atom => a
      case n: Negation => n
    }
    rec(negationForm(f))
  }

  def conjunctiveForm(f: Formula): Formula = {
    def rec(f: Formula): Formula = f match {
      case Or(And(f1,f2), f3) => rec(f1 | f3) & rec(f2 | f3)
      case Or(f3, And(f1,f2)) => rec(f3 | f1) & rec(f3 | f2)
      case b: FBinary =>
        val nb = b.create(rec(b.f1), rec(b.f2))
        if(nb != b) rec(nb) else nb
      case a: Atom => a
      case n: Negation => n
    }
    rec(negationForm(f))
  }
}
