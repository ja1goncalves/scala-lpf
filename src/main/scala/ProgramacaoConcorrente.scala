import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

object ProgramacaoConcorrente {
  
   def fib(n:Int):Int = {
      if (n<=1)
        1
      else
        fib(n-1) + fib(n-2)
   }
   
   def pfib1(n:Int):Int = {
     var f1 = 0;
     var f2 = 0;
     val th1 = new Thread( () => {
        f1 = fib(n-2)
        println("fim")
     })
     th1.start
     
     f2 = fib(n-1)
     th1.join()
     return f1+f2
   }
   
   def pfib2(n:Int):Int = {
     implicit val ec = ExecutionContext.global;
     
     val f1 = Future {
       fib(n-1)
     }
     val f2 = Future {
       fib(n-2)
     }
     val soma = f1.zipWith(f2)(_+_)
     Await.result(f1, Duration.Inf);
   }
   def pfib3(n:Int):Int = {
     List(n-1,n-2).par.map(fib).sum
   }
   def main(args:Array[String]):Unit = {
     val inicio = System.currentTimeMillis();
     println("c fib(45) = " + pfib3(45))
     val fim = System.currentTimeMillis();
     println("tempo = " + ((fim-inicio)/1000.0))
   }
}