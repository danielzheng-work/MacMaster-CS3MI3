def isPrime(x : Int) : Boolean ={

    if(x >= 2){
        for( i <- 2 to (x-1)){

            if(x % i == 0) return false
        }
        return true 
    }
    return false

}

def isPalindrome[A](xs: List[A]) : Boolean = xs match{
    case Nil => true
    case List(x) => true
    case xs => {
        val reverseList = xs.reverse
        return reverseList.equals(xs)
    }
}


def digitList(x : Int) : List[Int] = {
    val stringInt = x.toString
    val list= stringInt.toList
    val ListInt = for(elem <- list) yield elem.asDigit
    return ListInt
}

def primePalindrome(x : Int) : Boolean = {
    return (isPrime(x) && isPalindrome(digitList(x)))
}



