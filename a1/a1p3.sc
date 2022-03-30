import scala.math.pow
// Representation of Expr
sealed trait VarExpr[+A]
case class Const[A](num: Int) extends VarExpr[A]
case class Neg[A](expr: VarExpr[A]) extends VarExpr[A]
case class Abs[A](expr: VarExpr[A]) extends VarExpr[A]
case class Plus[A](left: VarExpr[A], right: VarExpr[A]) extends VarExpr[A]
case class Times[A](left: VarExpr[A], right: VarExpr[A]) extends VarExpr[A]
case class Minus[A](left: VarExpr[A], right: VarExpr[A]) extends VarExpr[A]
case class Exp[A](left: VarExpr[A], right: VarExpr[A]) extends VarExpr[A]
//new constructors for substitution
case class Var[A](x: Symbol) extends VarExpr[A]
case class Subst[A](v:VarExpr[A], x:Symbol, num:VarExpr[A]) extends VarExpr[A]


// method for replacing Var type into Cons Type

def replaceOneVar[A]( exp: VarExpr[A], x : Symbol, c : VarExpr[A]) : VarExpr[A]  = exp match {
    case Neg(a) =>a match {
        case Var(expr) if expr.equals(x) => Neg(c)
        case _ => Neg(a)
    }
    case Abs(a) => a match {
        case Var(expr) if expr.equals(x) => Abs(c)
        case _ => Abs(a)
    }
    case Plus(a, b) => a match {
        case Var(expr) if expr.equals(x) => Plus(c,b)
        case _ => b match{
            case Var(expr) if expr.equals(x) => Plus(a,c)
            case _ => Plus(a,b)
        }
    }
    case Times(a, b) => a match {
        case Var(expr) if expr.equals(x) => Times(c,b)
        case _ => b match{
            case Var(expr) if expr.equals(x) => Times(a,c)
            case _ => Times(a,b)
        }
    }
    case Minus(a, b) => a match {
        case Var(expr) if expr.equals(x) => Minus(c,b)
        case _ => b match{
            case Var(expr) if expr.equals(x) => Minus(a,c)
            case _ => Minus(a,b)
        }
    }
    case Exp(a, b) => a match {
        case Var(expr) if expr.equals(x) => Exp(c,b)
        case _ => b match{
            case Var(expr) if expr.equals(x) => Exp(a,c)
            case _ => Exp(a,b)
        }
    }
}

// Interpreter

def interpretVarExpr[A](expr : VarExpr[A]) : Int = expr match {
    case Const(a) => a
    case Neg(a) => -(interpretVarExpr(a))
    case Abs(a) => (interpretVarExpr(a)).abs
    case Plus(a, b) => interpretVarExpr(a) + interpretVarExpr(b)
    case Times(a, b) => interpretVarExpr(a)*interpretVarExpr(b)
    case Minus(a, b) => interpretVarExpr(a)-interpretVarExpr(b)
    case Exp(a, b) => pow(interpretVarExpr(a),interpretVarExpr(b)).toInt

    // substitution 
    case Subst(e, x, c ) => e match{
        case Var(a) => interpretVarExpr(c)
        case Neg(a) => interpretVarExpr(replaceOneVar(e,x,c))
        case Abs(a) => interpretVarExpr(replaceOneVar(e,x,c))
        case Plus(a, b) => interpretVarExpr(replaceOneVar(e,x,c))
        case Times(a, b) => interpretVarExpr(replaceOneVar(e,x,c))
        case Minus(a, b) => interpretVarExpr(replaceOneVar(e,x,c))
        case Exp(a, b) => interpretVarExpr(replaceOneVar(e,x,c))
        //more than one variables need to substitute 
        case Subst(a, b, cons) => interpretVarExpr(Subst(replaceOneVar(a,b,cons),x,c))
    } 
}
