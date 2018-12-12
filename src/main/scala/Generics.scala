object Generics{
    def map[A](f: (A) => A, l: List[A]): List[A] = {
        l match{
            case Nil => Nil
            case head::tail=> f(head) :: map(f,tail)
        }
    }

    def filter[A](f: (A) => Boolean, l: List[A]): List[A] = {
        l match{
            case Nil => Nil
            case head::tail =>
                if(f(head)) head::filter(f,tail)
                else filter(f,tail)
        }
    }

    def take[A](n: Int, l: List[A]): List[A] = {
        l match {
            case Nil => Nil
            case head::tail =>
                if(n!=0) head::take(n-1,tail)
                else Nil
        }
    }

    def drop[A](n: Int, l: List[A]): List[A] = {
        l match {
            case Nil => Nil
            case head::tail =>
                if(n>0) drop(n-1,tail)
                else l
        }
    }

    // //TAKERIGHT:
    // def takeRight[A](n:Int,l:List[A]):List[A]={
    //   l match{
    //     case  
    //   }
    // }

    // //FOLD:
    // def fold[A](f:(A)=>A,n:Int,l:List[A]):A={
    //   l match{
    //     case Nil=>Nil
    //     case head::tail=> if(op=='+') head+n::fold(n,op,tail)
    //                       else if(op=='-')head-n::fold(n,op,tail)
    //                       else if(op=='*')head*n::fold(n,op,tail)
    //                       else head/n::fold(n,op,tail)
    //   }
    // }
    // println("fold"+fold(2,'+',List(1,2,3)))
}