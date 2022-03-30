import $file.h1, h1._
// Fill in your constructors here.

def BT_node[A](l: BinTree[A], a: A, r: BinTree[A]): BinTree[A] = Branch_BT(l,a,r)
def BT_leaf[A]: BinTree[A] = Leaf_BT()
def BT_flatten[A](t: BinTree[A]): List[A] = flatten[A](t)
def BT_orderedElems(t: BinTree[Int]): List[Int] = orderedElems(t)

def LT_node[A](l : LeafTree[A], r: LeafTree[A]): LeafTree[A] = Branch_LT(l,r)
def LT_leaf[A](a: A): LeafTree[A] = Leaf_LT(a)
def LT_flatten[A](t: LeafTree[A]): List[A] = flatten[A](t)
def LT_orderedElems(t: LeafTree[Int]): List[Int] = orderedElems(t)