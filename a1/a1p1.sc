import scala.math.pow
// Representation of Expr
sealed trait Expr[+A]
case class Const[A](num: Int) extends Expr[A]
case class Neg[A](expr: Expr[A]) extends Expr[A]
case class Abs[A](expr: Expr[A]) extends Expr[A]
case class Plus[A](left: Expr[A], right: Expr[A]) extends Expr[A]
case class Times[A](left: Expr[A], right: Expr[A]) extends Expr[A]
case class Minus[A](left: Expr[A], right: Expr[A]) extends Expr[A]
case class Exp[A](left: Expr[A], right: Expr[A]) extends Expr[A]

// Interpreter
def interpretExpr[A](expr: Expr[A]) : Int = expr match {
    case Const(a) => a
    case Neg(a) => -(interpretExpr(a))
    case Abs(a) => (interpretExpr(a)).abs
    case Plus(a, b) => interpretExpr(a) + interpretExpr(b)
    case Times(a, b) => interpretExpr(a)*interpretExpr(b)
    case Minus(a, b) => interpretExpr(a)-interpretExpr(b)
    case Exp(a, b) => pow(interpretExpr(a),interpretExpr(b)).toInt

}
