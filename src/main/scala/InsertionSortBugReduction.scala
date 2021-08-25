object InsertionSortBugReduction {
  import scala.language.reflectiveCalls
  import Program._

  val j = Var("j")
  var x = Var("x")
  val A = Var("A", PType.PArray)
  val A_j = Var("A_j")
  val A_i = Var("A_i")
  val A_i_plus_one = Var("A_i_plus_one")

  val p = Prog(
    x := A(1),
    A(1) := A_j,
    A_i := A(1),
    Assert(A_i === x)

    // x := A(1),
    // j := 0,
    // A_j := A(0),
    // A(j + 1) := A_j,
    // A(j) := x,
    // A_i := A(0),
    // A_i_plus_one := A(1),
    // Assert(A_i <= A_i_plus_one)
  )

}

object InsertionSortBugRunner extends App {
  val symex = new SymEx(IntExprEncoder, new Z3SMT)
  import InsertionSortBugReduction.{j, x, A, A_i, A_i_plus_one, A_j}
  symex.smt.logCommands(true)
  symex.exec(
    InsertionSortBugReduction.p,
    List(j, x, A, A_i, A_i_plus_one, A_j),
    200
  )
}
