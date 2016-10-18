import logic._

val example = "P∧Q → P → Q"

val style = FormulaPrintStyle.programming

val p = new FormulaParser(style)

p("a -> (  c   -> !  d) & a")

//p("(x)")