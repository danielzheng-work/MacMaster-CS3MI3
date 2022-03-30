import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}

def summingPairs(xs: Vector[Int], sum: Int): Future[Vector[Tuple2[Int,Int]]] = {
  def summingPairsHelper(xs: Vector[Int],
                         the_pairs: Vector[Tuple2[Int,Int]]): Future[Vector[Tuple2[Int,Int]]] =
    xs match {
      case fst +: rest =>
            val half1= rest.take(rest.length/2)
            val pair1 = half1.collect({case snd if fst + snd <= sum => (fst,snd)})
            val half2 = rest.takeRight(rest.length/2)
            val pair2 = half2.collect({case snd if fst + snd <= sum => (fst,snd)})
            val pairs= pair1++pair2
        // Search through `rest` for numbers `snd` such that `fst + snd` is the `sum`.
        // Make the recursive call, adding in the pairs we just found.
            summingPairsHelper(rest, the_pairs ++ pairs)
      case _ => Future{the_pairs} // If there's no head element, the vector is empty.
    }
  
  summingPairsHelper(xs,Vector())
}