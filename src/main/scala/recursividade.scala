object recursividade extends App {

  def fat(n: Int): Int = {
    if(n <= 1)
      1
    else
      fat(n-1)*n
  }

  def fib(n: Int): Int = {
    if(n <= 1)
      1
    else
      fib(n-1)+fib(n-2)
  }

  def repeat(n: Int, k: Int): Int = {
    if(n != 0){
      if(n%10 == k)
        repeat(n/10, k)+1
      else
        repeat(n/10, k)
    }else
      0
  }

  def convertIntForBinary(n: Int): Int = {
    if(n >= 2){
      if(n%2 == 0)
        convertIntForBinary(n/2)*10//+0
      else
        (convertIntForBinary(n/2)*10)+1
    }else
      n%2
  }

  def pascal(line: Int, column: Int): Int = {
    if((column == 1) || (1 == column))
      1
    else
      pascal(line-1, column)+pascal(line-1, column-1)
  }

  def MDC(m: Int, n:Int): Int = {
    if(n == 0)
      m
    else
      MDC(m, n%m)
  }
}