object Main extends App {

  //1
  def lassThanTwo(a: Int, b: Int):Int = {
    if(a > b) b
    else a
  }
  //2
  def lassThanTree(a: Int, b: Int, c: Int): Int = {
    if(a < b && a < c) a
    else if(b < a && b < c) b
    else c
  }
  //3
  def fat(n: Int):Int = {
    if(n <= 1) 1
    else n*fat(n-1)
  }
  //4
  def fibonacci(n: Int):Int = {
    if(n <= 1) 1
    else fibonacci(n-1) + fibonacci(n-2)
  }
  //5
  def position(l: List[Int], n: Int): Int = l(n)
  //6
  def belongsTo(list: List[Int], n: Int): Boolean = {
    if(list.isEmpty) false
    else{
      if(list.head == n) true
      else belongsTo(list.tail, n)
    }
  }
  //7
  def size(list: List[Int]): Int = {
    if(list.isEmpty) 0
    else 1+size(list.tail)
  }
  //8
  def max(list: List[Int]): Int = {
    if(list.isEmpty) -1
    else if(list.length == 1) list.head
    else{
      if(list.head > list.last) max(list.init)
      else max(list.tail)
    }
  }
  //9
  def contains(list: List[Int], element: Int): Int = {
    if(list.isEmpty) 0
    else{
      if(list.head == element) 1 + contains(list.tail, element)
      else contains(list.tail, element)
    }
  }
  //10
  def singleOccurrence(list: List[Int], element: Int): Boolean = {
    if(list.isEmpty) false
    else {
      if(list.head == element){
        if(!belongsTo(list, element)) true
        else false
      }else singleOccurrence(list.tail, element)
    }
  }
  //11
  def biggerThan(list: List[Int], num: Int): List[Int] = {
    if(list.isEmpty) List()
    else{
      if(list.head > num) List(list.head) ++ biggerThan(list.tail, num)
      else biggerThan(list.tail, num)
    }
  }
  //12
  def concatenate(list1: List[Int], list2: List[Int]): List[Int] = list1 ++ list2
  //13
  def removeFistOccurrence(list: List[Int], num: Int): List[Int] = {
    if(list.isEmpty) List()
    else{
      if(list.head == num) list.tail
      else List(list.head) ++ removeFistOccurrence(list.tail, num)
    }
  }
  //14
  def removeLastElement(list: List[Int]): List[Int] = {
    if(list.length == 1) List()
    else List(list.head) ++ removeLastElement(list.tail)
    //list.init
  }
  //15
  def removeRepeated(list: List[Int]): List[Int] = {
    if(list.isEmpty) list
    else{
      if(belongsTo(list.tail, list.head)) List(list.head) ++ removeRepeated(removeFistOccurrence(list.tail, list.head))
      else List(list.head) ++ removeRepeated(list.tail)
    }
  }
  //16
  def onTheList(list: List[Int], n: Int): List[Int] = {
    if(list.isEmpty) List()
    else{
      if(list.head > n){
        List(list.head) ++ onTheList(list.tail, n)
      }else onTheList(list.tail, n)
    }
  }
  //17
  def generateSequence(n: Int): List[Int] = {
    if(n == 1) List(1, -1)
    else generateSequence(n - 1) ++ List(n, -n)
  }
  //18
  def invert(list: List[Int]): List[Int] = {
    if(list.length == 1) list
    else invert(list.tail) ++ List(list.head)
  }
  //19
  type TuplaList = (List[Int], List[Int])

  def shareForN(list: List[Int], n: Int): TuplaList = (list.take(n), list.drop(n))

  def getStart(list: List[Int], n: Int): List[Int] = {
    if(list.size == n || list.isEmpty) list
    else getStart(list.init, n)
  }

  def getEnd(list: List[Int], n: Int): List[Int] = {
    if(n == 0) list
    else getEnd(list.tail, n - 1)
  }

  def shareForN_2(list: List[Int], n: Int): TuplaList = {
    if(n <= 0) (List(), list)
    else if(n > list.size) (list, List())
    else (getStart(list, n), getEnd(list, n))
  }
  //20
  def intercalate(l1: List[Int], l2: List[Int]): List[Int] = {
    if(l1.isEmpty) l2
    else if (l2.isEmpty) l1
    else{
      if(l1.length >= l2.length) List(l1.head) ++ List(l2.head) ++ intercalate(l1.tail, l2.tail)
      else List(l2.head) ++ List(l1.head) ++ intercalate(l1.tail, l2.tail)
    }
  }
  //21
  def unionNotRepeat(l1: List[Int], l2: List[Int]): List[Int] = {
    if(l1.isEmpty) l2
    else if(l2.isEmpty) l1
    else{
      if(!l1.contains(l2.head)) l1 ++ List(l2.head) ++ unionNotRepeat(l1 ++ List(l2.head), l2.tail)
      else l1 ++ unionNotRepeat(l1, l2.tail)
    }
  }
  //22
  def intersection(l1: List[Int], l2: List[Int]): List[Int] = {
    if(l1.isEmpty) l2
    else if(l2.isEmpty) l1
    else{
      if(l1.contains(l2.head)) List(l2.head) ++ intersection(l1, l2.tail)
      else intersection(l1, l2.tail)
    }
  }
  //23
  def sequence(x: Int, y: Int): List[Int] = {
    if(x == 0) List()
    else if(x == 1) List(y)
    else List(y) ++ sequence(x - 1, y + 1)
  }
  //24
  def insertTidy(list: List[Int], n: Int): List[Int] = {
    if(list.isEmpty) List(n)
    else{
      if(list.head < n) List(list.head) ++ insertTidy(list.tail, n)
      else List(n) ++ list
    }
  }
  //25
  def isOrdered(l: List[Int]): Boolean = {
    val sec = l.tail
    if(l.head >= sec.head) false
    else {
      if(!isOrdered(l.tail)) true
      else false
    }
  }
  //26
  def order(l: List[Int]): List[Int] = {
    if(l.size == 1) l
    else{
      val smallerValue: Int = selectSmaller(l)
      List(smallerValue) ++ order(removeSelect(smallerValue, l))
    }
  }

  def removeSelect(i: Int, list: List[Int]): List[Int] = {
    if(list.head == i) list.tail
    else List(list.head) ++ removeSelect(i, list.tail)
  }

  def selectSmaller(list: List[Int]): Int = {
    if(list.size == 1) list.head
    else{
      val rest: List[Int] = list.tail
      if(list.head < rest.head) selectSmaller(List(list.head) ++ rest.tail)
      else selectSmaller(rest)
    }
  }
  //27
  def toRotateLeft(l: List[Int], n: Int): List[Int] = {
    if(n <= 0) l
    else toRotateLeft(List(l.last) ++ l.init, n - 1)
  }
  //28
  def toRotateRigth(list: List[Int], n: Int): List[Int] = {
    if(n <= 0) list
    else toRotateRigth(List(list.head) ++ list.tail, n - 1)
  }
  //29
  def strtoupper(x: String): String = {
    if(x.isEmpty) x
    else toupper(x.head) +: strtoupper(x.tail)
  }
  def toupper(x: Char): Char = {
    if (x >= 'a' && x <= 'z') (x - 32).toChar
    else x
  }
  def lower(x: Char): Char = {
    if(x >= 'A' && x <= 'Z') (x + 32).toChar
    else x
  }
  //30
  def firstToupper(str: String): String = {
    if(str.length == 1) toupper(str.head).toString
    else{
      val _str: String = str.init
      if(_str.last == ' ') firstToupper(str.init) + toupper(str.last).toString
      else firstToupper(str.init) + lower(str.last).toString
    }
  }
  //31
  def positions(string: String, pos: List[Int]): List[Char] = {
    if(pos.size == 1) List(walkString(string, pos.head - 1))
    else List(walkString(string, pos.head - 1)) ++ positions(string, pos.tail)
  }

  def walkString(str: String, i: Int): Char = {
    if(i == 0) str.head
    else walkString(str.tail, i - 1)
  }
  //32
  def palindrome(str: String): Boolean = {
    if(str.length%2 == 0) palindromePair(str)
    else palindromeOdd(str)
  }
  def palindromePair(str: String): Boolean = {
    if(str.head != str.last) false
    else if(str.length > 2){
      val _str: String = str.tail
      palindromePair(_str.init)
    }
    else true
  }
  def palindromeOdd(str: String): Boolean = {
   if(str.length == 1) true
   else if(str.head != str.last) false
   else {
     val _str: String = str.tail
     palindromeOdd(_str.init)
   }
  }
  //33
  def isPrime(n: Int, seq: Int): Boolean = {
    if(seq - 1 == 1 || n == 1) true
    else {
      if (n%(seq-1) != 0) isPrime(n, seq-1)
      else false
    }
  }
  //34
  def sumDigits(n: Int): Int = {
    sumString(n.toString)
  }
  def sumString(str: String): Int = {
    if(str.length == 1) str.toInt
    else (str.head.toInt - 48) + sumString(str.tail)
  }
  //35
  def bubbleSort(list: List[Int]): List[Int] = {
    if(isOrdered(list)) list
    else {
      if(list.head > list.tail.head) bubbleSort(List(list.tail.head) ++ bubbleSort(removeSelect(list.tail.head, list)))
      else  bubbleSort(List(list.head) ++ bubbleSort(list.tail))
    }
  }
  //36
  def toCompact(list: List[Int]): List[List[Int]] = {
    if(list.isEmpty) List(List())
    else{
      val repeat: List[Int] = compact(list.head, list, 0)
      if(repeat.length == 1) repeat +: toCompact(list.tail)
      else repeat +: toCompact(listAfterCases(list, repeat.head))
    }
  }
  def compact(n: Int, l: List[Int], x: Int): List[Int] = {
    if(l.head == n) compact(n, l.tail, x+1)
    else  {
      if(x == 0) List(n)
      else List(n, x)
    }
  }
  def listAfterCases(list: List[Int], n: Int): List[Int] = {
    if(n == 0) list
    else listAfterCases(list.tail, n-1)
  }
  println(firstToupper("joao paAUlo felix"))

}