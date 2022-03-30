import $file.a2_ulterm, a2_ulterm._

sealed trait STType
case object STNat extends STType {
  override def toString() = "nat"
}
case object STBool extends STType {
  override def toString() = "bool"
}

// Functions have a domain type and a codomain type.
case class STFun(dom: STType, codom: STType) extends STType {
  override def toString() = "(" + dom.toString + ") -> (" + codom.toString + ")"
}
// Example use: the type "nat -> bool" is written STFun(STNat,STBool)

// Part 1
sealed trait STTerm
case class STVar(index : Int) extends STTerm{
    override def toString() = index.toString()
}
case class STApp(t1: STTerm, t2: STTerm) extends STTerm {
    override def toString() = "(" + t1.toString() + ") (" + t2.toString() + ")"
}
case class STAbs(v: STType, t: STTerm) extends STTerm {
    override def toString() = "lambda : " + v.toString() + " . " + t.toString()
}
case object STZero extends STTerm {
    override def toString() = "zero"
}
case class STSuc(t: STTerm) extends STTerm {
    override def toString() = "suc(" + t.toString() + ")"
}
case class STIsZero(t: STTerm) extends STTerm {
    override def toString() = "iszero(" + t.toString() + ")"
}
case object STTrue extends STTerm{
    override def toString() = "true"
}
case object STFalse extends STTerm{
    override def toString() = "false"
}
case class STTest(t1: STTerm, t2: STTerm, t3: STTerm) extends STTerm {
    override def toString() = "(" + t1.toString() + ") (" + t2.toString() + ") (" + t3.toString() + ")"
}

//Part 2
def typecheck(term: STTerm) :Boolean = {
    def typeOf(t: STTerm, currentB: Int, list:List[Nothing]): Option[STType] = t match {
        case STVar(x) if (x > currentB) => None
        case STVar(x) => Some(STNat)
        case STTrue => Some(STBool)
        case STFalse => Some(STBool)
        case STZero => Some(STNat)
        case STSuc(t2) => typeOf(t2,currentB, list) match {
            case Some(STNat) => Some(STNat)
            case _ => None
        }
        case STIsZero(t2) => typeOf(t2,currentB, list) match {
            case Some(STNat) => Some(STBool)
            case _ => None
        }
        case STAbs(v, t2) =>
            v match{
                case STFun(d, c) => t2 match{
                    case STAbs(v2, t3) if (v2 == d) => Some(STFun(STFun(d, c), STFun(v2,c)))
                    case STVar(x) if(x <= currentB+1)=> Some(STFun(v,v))
                    case _ => None  
                    }
                case _ => typeOf(t2, currentB+1, list) match{
                    case Some(STNat) => Some(STFun(v, STNat))
                    case Some(STBool) => Some(STFun(v, STBool))
                    case Some(STFun(d, c)) => Some(STFun(v, STFun(d, c)))
                    case _ => None
                } 
            }
        case STApp(t1, t2) => typeOf(t1,currentB, list) match {
            case Some(STFun(a,b)) if (typeOf(t2, currentB, list) == Some(a))=> Some(b)
            case _ => None 
        }

        case STTest(b, t1, t2) => 
            val fstTy = typeOf(t1, currentB, list)
            typeOf(b, currentB, list) match {
            case Some(STBool) => typeOf(t2, currentB, list) match{
                case Some(STNat) if (fstTy == Some(STNat)) => Some(STNat)
                case Some(STBool) if (fstTy == Some(STBool)) => Some(STBool)
                case Some(STFun(d, c)) if (fstTy == Some(STFun(d, c))) => Some(STFun(d, c))
                case None => None
            } 
            case _ => None
        }
    }
    typeOf(term, -1, Nil) match {
        case None => false
        case _ => true
    }

    
}

// Part 3

def eraseTypes(stT: STTerm) : ULTerm = stT match {
    case STVar(x) => ULVar(x)
    case STAbs(v, t) => ULAbs(eraseTypes(t))
    case STApp(t1, t2) => ULApp(eraseTypes(t1), eraseTypes(t2))
    case STTrue => ULAbs(ULAbs(ULVar(1)))
    case STFalse => ULAbs(ULAbs(ULVar(0)))
    case STTest(t1, t2, t3) => ULApp(ULApp(ULAbs(ULAbs(ULAbs(ULApp(ULApp(ULVar(2), ULVar(1)),ULVar(0))))),
        eraseTypes(t2)), eraseTypes(t3))
    case STZero => ULAbs(ULAbs(ULVar(0)))
    case STSuc(t) => ULApp(ULAbs(ULAbs(ULAbs(ULApp(ULVar(1),
        ULApp(ULApp(ULVar(2),ULVar(1)),ULVar(0)))))), eraseTypes(t))
    case STIsZero(t) => ULApp(ULAbs(ULApp(ULApp(ULVar(0),ULAbs(eraseTypes(STFalse))),eraseTypes(STTrue))), eraseTypes(t))
}