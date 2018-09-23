object  alta_ordem extends App {
//  def pot(f: Int => Int, k: Int): Int => Int = (x: Int) => {
//    if(k > 0){
//      f(pot(f, k-1))
//    }else
//      f(x)
//  }

  def derivative(f: Double => Double, d: Double) = (x: Double) => (f(x+d)-f(x))/d

  def biggerValue(f: Int => Int, g: Int => Int) = (x: Int) => {
    if(f(x) > g(x))
      f(x)
    else
      g(x)
  }

  def medialValue(f: Int => Int, g: Int => Int) = (x: Int) => {
    (f(x) + g(x))/2
  }

  def sum(f: Int => Int, max: Int, min: Int):Int = {
    if(min == max)
      f(min)
    else
      f(max) + sum(f, max-1, min)
  }

  def elementPA(n: Int, r: Int, start: Int): Int = {
    if(n == 0)
      start
    else
      elementPA(n-1, r, start) + r
  }

  def porPA(f: Int => Int, start: Int, n: Int): Int = {
    if(n == 0)
      start
    else
      f(porPA(f, start, n - 1))
  }

  def hanoi(discos: Int): Int = {
    if(discos <= 3)
      7
    else
      2 * hanoi(discos - 1) + 1
  }

}
