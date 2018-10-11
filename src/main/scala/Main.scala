object Main extends App {
  //https://scalafiddle.io/sf/fBGr6N7/0

  //***************************************************************************************
  //99 - List

  //P04
  def length(l: List[Int]): Int ={
    l match{
      case head::tail => 1 + length(tail)
      case Nil => 0
    }
  }
  //P05
  def reverse(l: List[Int]): List[Int] ={
    l match {
      case head::tail => reverse(tail) ++ List(head)
      case Nil => Nil
    }
  }

  //P07
  def flatten(l: Any): List[Any] = {
    l match {
      case b:List[_] => b match {
        case head::tail => flatten(head) ++ flatten(tail)
        case Nil => Nil
      }
      case b:Int => List(b)
    }
  }
  //P08
  def compress(l: List[Any]): List[Any] = {
    l match{
      case head::next::tail => if(head==next) compress(next::tail)
      else List(head):::compress(next::tail)
      case head::tail => List(head)
      case Nil => Nil
    }
  }
  //P14
  def duplicate(list: List[Any]): List[Any] = {
    list match {
      case head::tail => List(head, head):::duplicate(tail)
      case Nil => Nil
    }
  }
  //****************************************************************************************************
  //Luis propos FLAT
  def flat(list: List[List[Any]]): List[Any] = {
    list match {
      case head::tail => auxFlat(head) ++ flat(tail)
      case Nil=> Nil
    }
  }

  def auxFlat(list: List[Any]): List[Any] ={
    list match {
      case head::tail => List(head):::auxFlat(tail)
      case Nil => Nil
    }
  }

  //Luis propos CONTA
  type Tupla = (Int, Int)
  def count(list: List[Int]): List[Tupla] = {
    list match {
      case head::tail => val tupla: Tupla = (head,countOccurrences(head,list))
        tupla::count(removeAll(head,tail))
      case Nil => Nil

    }
  }

  def countOccurrences(num: Int, l: List[Int]): Int = {
    l match {
      case head::tail => if(head == num) 1+countOccurrences(num,tail)
      else countOccurrences(num, tail)
      case Nil => 0
    }
  }

  def removeAll(num: Int, l: List[Int]): List[Int] = {
    l match {
      case head::tail => if(head == num) removeAll(num,tail)
      else List(head):::removeAll(num,tail)
      case  Nil => Nil
    }
  }

  //Luis propos ZIP
  def zip(l1: List[Int], l2: List[Int]): List[Tupla] = {
    (l1,l2) match {
      case (Nil,_) => Nil
      case (_,Nil) => Nil
      case(xh::xt,yh::yt) => List((xh,yh)):::zip(xt,yt)
    }
  }



  //*********************************************************************************************
  //Prova 2018.1

  //Questão 1)
  def friednlyNumbers(x:Int, y:Int):Boolean = {
    if(digitsSum(x) == digitsSum(y))
      true
    else
      false
  }

  def digitsSum(n:Int): Int = {
    divNum(n)
  }

  def divNum(num:Int):Int = {
    if(num < 10)
      num
    else
      num%10 + divNum(num/10)
  }

  //Questão 2)
  def subList(list:List[Int], first: Int, last: Int): List[Int] = {
    list match {
      case head::tail => if(first == head) List(head):::completeList(tail, last)
      else subList(tail, first, last)
      case Nil => Nil
    }
  }

  def completeList(list:List[Int], lastValue: Int): List[Int] = {
    list match {
      case head::tail => if(head != lastValue) List(head):::completeList(tail, lastValue)
      else List(head)
      case Nil => Nil
    }
  }


}
