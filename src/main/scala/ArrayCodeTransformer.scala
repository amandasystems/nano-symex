object ArrayCodeTransformer {
  import Program._

  var variableIndex = 0

  private def freshVariable(prefix: String): Var = {
    val name = s"${prefix}_${variableIndex}"
    variableIndex += 1
    Var(name)
  }

  private def replaceArrayAccessVisitor(
      e: BExpr
  ): (Prog, BExpr, Seq[Var]) =
    e match {
      case Eq(left, right) => {
        val (lAssigns, lExpr, lVars) = replaceArrayAccessVisitor(left)
        val (rAssigns, rExpr, rVars) = replaceArrayAccessVisitor(right)

        (Sequence(lAssigns, rAssigns), Eq(lExpr, rExpr), lVars ++ rVars)
      }
    }

  private def replaceArrayAccessVisitor(e: Expr): (Prog, Expr, Seq[Var]) =
    e match {
      case arrayAccess: ArrayElement => {
        val arrayVarTemp = freshVariable(arrayAccess.name)
        (
          Assign(arrayVarTemp, arrayAccess),
          arrayVarTemp,
          Seq(arrayVarTemp)
        )
      }
      case i: IntConst => (Skip, i, Seq())
      case v: Var      => (Skip, v, Seq())
    }

  def transformArrayCode(
      p: Prog,
      vars: Seq[Var],
      rewrittenProgram: Prog
  ): (Prog, Seq[Var]) = p match {
    case Sequence(Sequence(p1, p2), p3) =>
      transformArrayCode(Sequence(p1, Sequence(p2, p3)), vars, rewrittenProgram)

    // Compile away skips
    case Skip => (rewrittenProgram, vars)
    case Sequence(Skip, rest) =>
      transformArrayCode(rest, vars, rewrittenProgram)

    case Sequence(Assert(cond), rest) => {
      val (assignTemporaries, rewrittenCond, newVars) =
        replaceArrayAccessVisitor(
          cond
        )

      val replacementStatements =
        Sequence(assignTemporaries, Assert(rewrittenCond))
      transformArrayCode(
        rest,
        vars ++ newVars,
        Sequence(rewrittenProgram, replacementStatements)
      )
    }

    case Sequence(Assign(lhs: ArrayElement, rhs: ArrayElement), rest) => {
      val rhsTemp = freshVariable(rhs.name)
      val assignTemp = Assign(rhsTemp, rhs)
      val rewrittenAssignment = Assign(lhs, rhsTemp)

      val replacementProgram = Sequence(assignTemp, rewrittenAssignment)
      println(replacementProgram)
      transformArrayCode(
        rest,
        vars appended rhsTemp,
        Sequence(rewrittenProgram, replacementProgram)
      )
    }

    // TODO while!

    case Sequence(assign @ Assign(_, _: Var), rest) => {
      transformArrayCode(rest, vars, Sequence(rewrittenProgram, assign))
    }
    
    case Sequence(prog, rest) => {
      transformArrayCode(rest, vars, Sequence(rewrittenProgram, prog))
    }

    case p =>
      transformArrayCode(Sequence(p, Skip), vars, rewrittenProgram)
  }

}
