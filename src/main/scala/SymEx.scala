import scala.language.implicitConversions

/** Extremely simple symbolic execution engine.
  */
class SymEx(encoder: ExprEncoder, spawnSMT: => SMT) {

  val smt = spawnSMT

  import encoder._
  import Program._
  import PType.{PInt, PArray}
  import smt._

  def shutdown() = smt.shutdown()

  private val noIndicesKnown = Map[BigInt, String]()

  smt.logCommands(false)

  def exec(p: Prog, variables: Seq[Var], depth: Int = Integer.MAX_VALUE) = {
    for (Var(name, PInt) <- variables) {
      declareConst(name, IntType)
    }
    val store = (for (v @ Var(name, PInt) <- variables) yield v -> name).toMap
    val arrayVarToIndexToConstant =
      (for (v @ Var(_, PArray) <- variables) yield v -> noIndicesKnown).toMap
    execHelp(p, List(), depth)(store, arrayVarToIndexToConstant)
    reset()
  }

  private implicit def arrayElementToVariable(
      arrayIndexExpression: ArrayElement
  ) = Var(arrayIndexExpression.name, PArray)

  private def getOrCreateArrayIndex(
      arrayVar: Var,
      index: BigInt,
      arrayStore: ArrayStore
  ): ArrayStore = {
    val arrayIndexToConstant = arrayStore(arrayVar)
    val arraySolverConstant =
      arrayIndexToConstant.getOrElse(index, freshConst(IntType))
    // We can survive executing this even if it is a no-op.
    val updatedIndexForArray =
      arrayIndexToConstant.updated(index, arraySolverConstant)

    arrayStore.updated(
      arrayVar,
      updatedIndexForArray
    )
  }

  def execHelp(p: Prog, ops: List[Prog], depth: Int)(implicit
      store: SymbStore,
      arrayVarToIndexToConstant: ArrayStore
  ): Unit = p match {

    case _ if ops.size > depth => ()

    case Skip => ()

    case Sequence(Skip, rest) =>
      execHelp(rest, ops, depth)

    case Sequence(Sequence(p1, p2), p3) =>
      execHelp(Sequence(p1, Sequence(p2, p3)), ops, depth)

    case Sequence(Assign(_: ArrayElement, _: ArrayElement), _) => ???

    case s @ (Sequence(Assign(_: ArrayElement, _), _) |
        Sequence(Assign(_: Var, _: ArrayElement), _)) => {
      val Sequence(assignment: Assign, rest) = s

      val arrayIndexExpression = assignment match {
        case Assign(_, rhs: ArrayElement) => rhs
        case Assign(lhs: ArrayElement, _) => lhs
      }

      // This is essentially the happy path of the array logic
      def performAssignment(indexValue: BigInt) = {
        val arrayStoreWithIndex =
          getOrCreateArrayIndex(
            arrayIndexExpression,
            indexValue,
            arrayVarToIndexToConstant
          )

        val assignedFreshConst = freshConst(IntType)

        val (symbolicRhs, varToConstant, arrayVarStore) =
          assignment match {
            case Assign(lhs: Var, rhs: ArrayElement) => {
              val storeWithAssignment = store.updated(lhs, assignedFreshConst)
              val rhsSolverConstant = arrayStoreWithIndex(rhs)(indexValue)
              (
                rhsSolverConstant,
                storeWithAssignment,
                arrayStoreWithIndex
              )
            }
            case Assign(lhs: ArrayElement, rhs) => {
              val rhsSMTSymbol = rhs match {
                case rhs: Var        => store(rhs)
                case IntConst(value) => value
                case _ =>
                  new RuntimeException(
                    s"complex expression ${rhs} not accepted!"
                  )
              }
              val lhsIndices = arrayStoreWithIndex(lhs)
              (
                rhsSMTSymbol,
                store,
                arrayStoreWithIndex.updated(
                  lhs,
                  lhsIndices.updated(indexValue, assignedFreshConst)
                )
              )
            }
          }

        addAssertion(s"(= ${assignedFreshConst} ${symbolicRhs})")
        execHelp(rest, assignment :: ops, depth)(varToConstant, arrayVarStore)
      }

      arrayIndexExpression.atIndex match {
        case IntConst(indexValue) => performAssignment(indexValue)
        case expressionOrVar => {
          val indexConst = expressionOrVar match {
            case v: Var => store(v)
            case expression => {
              // note: this will error if the expression is in LHS since it
              // can't be encoded.
              val newConst = freshConst(IntType)
              addAssertion(s"(= ${newConst} ${encode(expression)})")
              newConst
            }
          }
          val indexValue = {
            assert(isSat)
            smt.getSatValue(indexConst)
          }
          val indexCondition = s"(= ${indexConst} ${indexValue})"
          branchWithCondition(indexCondition) {
            // Continue on the happy path assuming this index is the one
            performAssignment(indexValue)
          } {
            // Retry with a different index
            execHelp(p, ops, depth)
          }
        }
      }
    }

    case Sequence(op @ Assign(lhs: Var, rhs), rest) => {
      val newConst = freshConst(IntType)
      addAssertion("(= " + newConst + " " + encode(rhs) + ")")
      val newStore = store + (lhs -> newConst)
      execHelp(rest, op :: ops, depth)(newStore, arrayVarToIndexToConstant)
    }

    case Sequence(IfThenElse(cond, b1, b2), rest) =>
      branchWithCondition(encode(cond)) {
        execHelp(Sequence(b1, rest), ops, depth)
      } {
        execHelp(Sequence(b2, rest), ops, depth)
      }

    case Sequence(w @ While(cond, body), rest) =>
      execHelp(
        Sequence(IfThenElse(!cond, Skip, Sequence(body, w)), rest),
        ops,
        depth
      )

    case Sequence(a @ Assert(cond), rest) => {
      push()
      addAssertion(encode(!cond))
      if (isSat) {
        println("Found path leading to failing assertion:")
        for (op <- (a :: ops).reverse) {
          println("  " + op)
        }

      }
      pop()
      execHelp(rest, ops, depth)
    }

    case p =>
      execHelp(Sequence(p, Skip), ops, depth)

  }

}

object SymExTest extends App {

  import ExampleProg._

  val symex = new SymEx(IntExprEncoder, new Z3SMT)

  symex.exec(p, List(a, b, x, y))

}

object SymExTest2 extends App {

  import ExampleProg2._

  val symex = new SymEx(IntExprEncoder, new Z3SMT)

  symex.exec(p, List(a, x), 200)

}
