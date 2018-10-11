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

  def numberFriends(x: Int, y: Int): Boolean = {
    def friends(num: String): Int = {
      num match {
        case "" => 0
        case _ => num.head.asInstanceOf[Int] + friends(num.tail) - 48
      }
    }

    def otherFriends(num: Int): Int = {
      val n: Int = num/10
      val rest: Int = num%10
      if(n == 0) num
      else rest + otherFriends(n)
    }
    otherFriends(x) == otherFriends(y)
  }

  def sub_list(list: List[Int], i: Int, f: Int): List[Int] = {
    def count(list: List[Int], i: Int, f: Int, c: Int): List[Int] = {
      list match {
        case Nil => Nil
        case head::tail =>
          if(c >= i && c <= f) head::count(tail, i, f, c+1)
          else count(tail, i, f, c+1)
      }
    }
    count(list, i, f, 0)
  }

  def almostDecreasing(list: List[Int]): Boolean = {
    def almostDecreasingCount(list: List[Int], count: Int): Boolean = {
      list match {
        case Nil => if(count == 1) true else false
        case head::Nil => if(count == 1) true else false
        case head::next::Nil => if(count == 1) true else false
        case head::next::tail =>
          if(head > next) almostDecreasingCount(next::tail, count)
          else almostDecreasingCount(next::tail, count+1)
      }
    }
    almostDecreasingCount(list, 0)
  }
}
