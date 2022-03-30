import scala.math.pow
// Representation of MixedExpr
sealed trait MixedExpr[+A]
case class Const[A](num: Int) extends MixedExpr[A]
case class Neg[A](ex: MixedExpr[A]) extends MixedExpr[A]
case class Abs[A](ex: MixedExpr[A]) extends MixedExpr[A]
case class Plus[A](left: MixedExpr[A], right: MixedExpr[A]) extends MixedExpr[A]
case class Times[A](left: MixedExpr[A], right: MixedExpr[A]) extends MixedExpr[A]
case class Minus[A](left: MixedExpr[A], right: MixedExpr[A]) extends MixedExpr[A]
case class Exp[A](left: MixedExpr[A], right: MixedExpr[A]) extends MixedExpr[A]

// Add constructors
case object TT extends MixedExpr[Nothing]
case object FF extends MixedExpr[Nothing]
case class Bnot[A](b : MixedExpr[A]) extends MixedExpr[A]
case class Band[A](bl: MixedExpr[A], br: MixedExpr[A])extends MixedExpr[A]
case class Bor[A](bl: MixedExpr[A], br: MixedExpr[A])extends MixedExpr[A]


// interpreter handle Int
def interpreterInt[A](exp: MixedExpr[A]) : Int = exp match{
    case Const(a) => a 
    case Neg(a) => -(interpreterInt(a))
    case Abs(a) => (interpreterInt(a).abs)
    case Plus(a, b) => (interpreterInt(a)+interpreterInt(b))
    case Times(a, b) => (interpreterInt(a)*interpreterInt(b))
    case Minus(a, b) => interpreterInt(a)-interpreterInt(b)
    case Exp(a, b) => pow(interpreterInt(a),interpreterInt(b)).toInt
}
// interpreter handle Boolean
def interpreterBoo[A](exp: MixedExpr[A]) : Boolean = exp match{
    case TT => true 
    case FF => false
    case Bnot(a) => !interpreterBoo(a)
    case Band(a, b) => interpreterBoo(a) && interpreterBoo(b)
    case Bor(a, b) => interpreterBoo(a) || interpreterBoo(b)
}

// Interpreter check if expression valid
def interpretMixedExpr[A](exp: MixedExpr[A]) : Option[Either[Int, Boolean]] = exp match {
    case Const(a) => Some(Left(interpreterInt(exp)))
    case Neg(a) => interpretMixedExpr(a) match{
        case Some(Left(_)) => Some(Left(interpreterInt(exp)))
        case _ => None
    }
    case Abs(a) => interpretMixedExpr(a) match{
        case Some(Left(_)) => Some(Left(interpreterInt(exp)))
        case _ => None
    }
    case Plus(a,b) => interpretMixedExpr(a) match{
        case Some(Left(_)) => interpretMixedExpr(b) match{
            case Some(Left(_)) => Some(Left(interpreterInt(exp)))
            case _ => None
        }
        case _ => None
    }
    case Times(a,b) => interpretMixedExpr(a) match{
        case Some(Left(_)) => interpretMixedExpr(b) match{
            case Some(Left(_)) => Some(Left(interpreterInt(exp)))
            case _ => None
        }
        case _ => None
    }
    case Minus(a,b) => interpretMixedExpr(a) match{
        case Some(Left(_)) => interpretMixedExpr(b) match{
            case Some(Left(_)) => Some(Left(interpreterInt(exp)))
            case _ => None
        }
        case _ => None
    }
    case Exp(a,b) => interpretMixedExpr(a) match{
        case Some(Left(_)) => interpretMixedExpr(b) match{
            case Some(Left(_)) => Some(Left(interpreterInt(exp)))
            case _ => None
        }
        case _ => None
    }

    case TT => Some(Right(interpreterBoo(exp)))
    case FF => Some(Right(interpreterBoo(exp)))

    case Bnot(a) => interpretMixedExpr(a) match{
        case Some(Right(_)) => Some(Right(interpreterBoo(exp)))
        case _ => None
    }
    case Band(a,b) => interpretMixedExpr(a) match{
        case Some(Right(_)) => interpretMixedExpr(b) match{
            case Some(Right(_)) => Some(Right(interpreterBoo(exp)))
            case _ => None
        }
        case _ => None
    }
    case Bor(a,b) => interpretMixedExpr(a) match{
        case Some(Right(_)) => interpretMixedExpr(b) match{
            case Some(Right(_)) => Some(Right(interpreterBoo(exp)))
            case _ => None
        }
        case _ => None
    }

}
