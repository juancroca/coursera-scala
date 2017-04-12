package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  val path = Var(List[String]())
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.mapValues{ exp =>
      Signal(eval(exp(), namedExpressions))
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    def binop(op : (Double, Double) => Double)(a: Expr, b: Expr) = op(eval(a, references), eval(b, references))
    expr match{
      case Literal(v) if path().contains(v) => Double.NaN
      case Literal(v) => v
      case Ref(name) => {
        val p = path()
        path.update(p.::(name))
        eval(getReferenceExpr(name, references), references)
      }
      case Plus(a, b) => binop(_ + _)(a, b)
      case Minus(a, b) => binop(_ - _)(a, b)
      case Times(a, b) => binop(_ * _)(a, b)
      case Divide(a, b) => binop(_ / _)(a, b)
    }
  }


  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
