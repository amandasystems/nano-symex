object SMTTest extends App {

  for (smt <- List(new Z3SMT))
    try {
      import smt._
      println("Testing SMT solver " + name + " ...")

      declareConst("x", "Int")
      declareConst("y", "Int")

      addAssertion("(> x y)")

      declareConst("a", "(Array Int Int)")
      addAssertion("(= (select a 1) 17)")
      println(isSat)
      println(getArrayValues("a"))

    } finally {
      smt.shutdown()
    }

}
