object MinimalArrayExample extends App {
  import scala.language.reflectiveCalls
  import ArrayCodeTransformer.transformArrayCode
  import Program._

  val A = Var("A", PType.PArray)
  val x = Var("x")

  val program = Prog(
    x := 0,
    A(1) := 2,
    A(2) := A(x + 1),
    Assert(A(2) === 2)
  )

  val symex = new SymEx(IntExprEncoder, new Z3SMT)
  symex.smt.logCommands(true)
  val (expandedProgram, newVars) = transformArrayCode(program, List(A, x), Skip)
  println("BEGIN EXPANDED PROGRAM")
  println(expandedProgram)
  println("END OF EXPANDED PROGRAM")
  symex.exec(expandedProgram, newVars, 200)
}
