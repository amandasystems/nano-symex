object MinimalArrayExample extends App {
  import scala.language.reflectiveCalls
  import Program._

  val A = Var("A", PType.PArray)
  val x = Var("x")

  val program = Prog(
    x := 1,
    A(1) := 2,
    A(2) := 2,
    Assert(A(1) === A(x + 1))
  )

  val symex = new SymEx(IntExprEncoder, new Z3SMT)
  symex.exec(program, List(A), 200)
}
