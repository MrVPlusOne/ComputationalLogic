import logic._

val example = "P∧Q → P → Q"

val style = FormulaPrintStyle.programming

val p = new NewParser(style)

p("a -> (c -> !d)")

//p("(x)")