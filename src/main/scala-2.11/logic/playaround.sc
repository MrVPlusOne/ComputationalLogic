import logic.Implicits._
import logic._
import NormalForm._

implicit val style = FormulaPrintStyle.textBook

val f = formula(!(!('P & 'Q) -> !'R)) //¬(P → ¬(P ∧ Q))

f.nnf
f.cnf
f.dnf

// Truth Table
TruthTable.create(f).printTable()
TruthTable.create(negationForm(f)).printTable()

formula(('P -> 'Q) & 'P & !'Q).substitute('P ~> FF).nnf.evaluate
TruthTable.create(formula(('P -> 'Q) & 'P & !'Q)).printTable()

formula('F1 & 'F2)

val f1=formula((!'P |'Q|'R) & (!'Q|'R) & (!'Q | !'R) & ('P | !'Q | !'R))
f1.substitute('Q ~> FF).evaluate

