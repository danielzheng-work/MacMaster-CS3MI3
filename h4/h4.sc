sealed trait Stream[+A]
case object SNil extends Stream[Nothing]
case class Cons[A](a: A, f: Unit => Stream[A]) extends Stream[A]

// Part 1 : Filtering Streams

def filter[A](p :  A => Boolean, s : => Stream[A]) : Stream[A] = s match {
    case SNil => SNil
    case Cons(a, f) => a match {
        case a if p(a) => Cons(a, _ => filter(p, f()))
        case _ => filter(p, f())
    }
}

// Part 2 : Zipping and Merging Streams

// zip streams
def zip[A](a : => Stream[A] , b : => Stream[A]) : Stream[(A,A)] = a match {
    case SNil => b match{
        case SNil  => SNil
        case Cons(y, g) => Cons((null.asInstanceOf[A], y), _ => zip(SNil,g()))
    }
    case Cons(x, f) => b match {
        case SNil => Cons((x, null.asInstanceOf[A]), _ => zip(f(), SNil))
        case Cons(y, g) => Cons((x, y), _ => zip(f(),g()))
    }
}

// merge streams 
def merge[A](a : => Stream[A] , b : => Stream[A]) : Stream[A] = a match{
    case SNil => b
    case Cons(x, f) => Cons(x, _ => merge(b, f()))
}

// Part 3 Quantifying over streams

// all predicate definition
def all[A](p : A => Boolean, s : => Stream[A]) : Boolean = s match {
    case SNil => true
    case Cons(a, f) => a match {
        case a if p(a) => all(p, f())
        case _ => false
    }
}

// exits predicate definition
def exists[A](p : A => Boolean, s : => Stream[A]) : Boolean = s match {
    case SNil => false
    case Cons(a, f) => a match {
        case a if p(a) => true
        case _ => exists(p, f())
    }
}

// Part 4 Tolerant zipping and merging of streams

// My original version of zip and merge is already taking care of returning sensible result without meet condition
// so I basically just copy entire implementation for this question with different name 
def zipSafe[A](a : => Stream[A] , b : => Stream[A]) : Stream[(A,A)] = a match {
    case SNil => b match{
        case SNil  => SNil
        case Cons(y, g) => Cons((null.asInstanceOf[A], y), _ => zipSafe(SNil,g()))
    }
    case Cons(x, f) => b match {
        case SNil => Cons((x, null.asInstanceOf[A]), _ => zipSafe(f(), SNil))
        case Cons(y, g) => Cons((x, y), _ => zipSafe(f(),g()))
    }
}


def mergeSafe[A](a : => Stream[A] , b : => Stream[A]) : Stream[A] = a match{
    case SNil => b
    case Cons(x, f) => Cons(x, _ => mergeSafe(b, f()))
}

