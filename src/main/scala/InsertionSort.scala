object InsertionSort {
  import scala.language.reflectiveCalls
  import Program._

  val i = Var("i")
  val j = Var("j")
  var x = Var("x")
  val A = Var("A", PType.PArray)

  val len = 7
  // A: 8 never finishes, 7 runs in 30 sek.

  val p = Prog(
    i := 2,
    While(i < len)(
      x := A(i),
      j := i - 1,
      While(j >= 0 & A(j) > x)(
        A(j + 1) := A(j),
        j := j - 1
      ),
      A(j + 1) := x,
      i := i + 1
    ),
    // Assertion: The final array is sorted
    i := 0,
    While(i + 1 < len)(
      Assert(A(i) <= A(i + 1)),
      i := i + 1
    )
  )

}

object InsertionSortRunner extends App {
  val symex = new SymEx(IntExprEncoder, new Z3SMT)
  import InsertionSort.{i, j, x, A}
  symex.exec(InsertionSort.p, List(i, j, x, A), 200)
}
