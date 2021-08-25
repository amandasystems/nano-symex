object InsertionSort {
  import scala.language.reflectiveCalls
  import Program._

  val i = Var("i")
  val j = Var("j")
  var x = Var("x")
  val A = Var("A", PType.PArray)
  val A_j = Var("A_j")
  val A_i = Var("A_i")
  val A_i_plus_one = Var("A_i_plus_one")

  val len = 8 // 6 = 9 sek, 7 takes 64 sek

  val p = Prog(
    i := 1,
    While(i < len)(
      x := A(i),
      j := i - 1,
      A_j := A(j),
      While(j >= 0 & A_j > x)(
        A(j + 1) := A_j,
        j := j - 1,
        A_j := A(j) // We have a new value for j, therefore A_j also changes
      ),
      A(j + 1) := x,
      i := i + 1
    ),
    // Assertion: The final array is sorted
    i := 0,
    While(i + 1 < len)(
      A_i := A(i),
      A_i_plus_one := A(i + 1),
      Assert(A_i <= A_i_plus_one),
      i := i + 1
    )
  )

}

object InsertionSortRunner extends App {
  val symex = new SymEx(IntExprEncoder, new Z3SMT)
  import InsertionSort.{i, j, x, A, A_i, A_i_plus_one, A_j}
  symex.smt.logCommands(false)
  symex.exec(InsertionSort.p, List(i, j, x, A, A_i, A_i_plus_one, A_j), 200)
}
