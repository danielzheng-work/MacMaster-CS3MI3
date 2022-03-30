//Part 1
    //LeafTree definition
    sealed trait LeafTree[A]
    case class Leaf_LT[A](node: A) extends LeafTree[A]
    case class Branch_LT[A](left: LeafTree[A], right: LeafTree[A]) extends LeafTree[A]
    
    //BinTree definition
    sealed trait BinTree[A]
    case class Leaf_BT[A] () extends BinTree[A]
    case class Branch_BT[A](left : BinTree[A], node: A, right: BinTree[A]) extends BinTree[A]

//Part 2

    //LeafTree to List
    def flatten[A](tree : LeafTree[A]) : List[A] = tree match {
        case Leaf_LT(value) => List(value)
        case Branch_LT(left, right) => flatten(left) ++ flatten(right)
    }

    //Bintree to List
    def flatten[A](tree : BinTree[A]) : List[A] = tree match {
        case Leaf_BT() => List()
        case Branch_BT(left, node, right) => flatten(left) ++ List(node) ++ flatten(right)
    }

//Part 3

    //LeafTree to sorted list
    def orderedElems(tree: LeafTree[Int]) : List[Int] =  { 
            return flatten[Int](tree).sorted       
    }

    //BinTree to sorted list
    def orderedElems(tree: BinTree[Int]) : List[Int] =  {
            return flatten[Int](tree).sorted
    }

//Part 4

    //StructTree Definition
    sealed trait StructTree[A, B]
    case class Leaf_ST[A,B](value : B) extends StructTree[A,B]
    case class Branch_ST[A,B](left : StructTree[A,B], node: A, right: StructTree[A, B]) extends StructTree[A,B]

//Part 5

    //StructTree to list
    def flatten[A, B](tree : StructTree[A, B]) : List[Any] = tree match {
        case Leaf_ST(value) => List(value)
        case Branch_ST(left, node, right) => flatten(left) ++ List(node) ++ flatten(right)
    }

