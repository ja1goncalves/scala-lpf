def map[A](f: (A) => A, s: Stream[A]): Stream[A] = {
  s match {
    case Stream.Empty => Stream.Empty
    case head#::tail=> f(head)# :: map(f,tail)
  }
}

def filter[A](f: (A) => Boolean s: Stream[A]): Stream[A] = {
  s match{
    case Stream.Empty => Stream.Empty
    case head#::tail =>
        if(f(head)) head#::filter(f,tail)
        else filter(f,tail)
  }
}

def take[A](n: Int, s: Stream[A]): Stream[A] = {
  s match {
    case Stream.Empty => Stream.Empty
    case head#::tail =>
        if(n!=0) head#::take(n-1,tail)
        else take(n-1,tail)
  }
}

def drop[A](n: Int, s: Stream[A]): Stream[A] = {
  s match {
    case Stream.Empty => Stream.Empty
    case head#::tail=>
        if(n!=0) drop(n-1,tail)
        else head#::drop(n-1,tail)
  }
}

def f(n:Int):Int = {
    println("calculando f("+n+")")
    println( 2*n+1)
    return 2*n+1

}
//Pega o valor inicial e final
def range(i:Int,s:Int):Stream[Int] = {
    println("range("+i+","+s+")")
    if (i==s)
        Stream(i)
    else
        i #:: range(i+1,s)
}

val r1 = range(1,1000).map(f).drop(10).take(5)
//println(r1)
// println(f(4))
// println(List(4))
// println(range(1,10).tail)