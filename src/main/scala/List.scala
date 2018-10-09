object List extends App {
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
    list match {
      case Nil => false
      case head::tail =>
        if(head == n) true
        else belongsTo(tail, n)
    }
  }
  //7
  def size(list: List[Int]): Int = {
    list match {
      case Nil => 0
      case head::tail => 1 + size(tail)
    }
  }
  //8
  def max(list: List[Int]): Int = {
    list match {
      case Nil => -1
      case head::Nil => head
      case head::next::tail =>
        if(head > next) max(head::tail)
        else max(next::tail)
    }
    //    if(list.isEmpty) -1
    //    else if(list.length == 1) list.head
    //    else{
    //      if(list.head > list.last) max(list.init)
    //      else max(list.tail)
    //    }
  }
  //9
  def contains(list: List[Int], element: Int): Int = {
    list match {
      case Nil => 0
      case head::tail =>
        if(head == element) 1 + contains(tail, element)
        else contains(tail, element)
    }
    //    if(list.isEmpty) 0
    //    else{
    //      if(list.head == element) 1 + contains(list.tail, element)
    //      else contains(list.tail, element)
    //    }
  }
  //10
  def singleOccurrence(list: List[Int], element: Int): Boolean = {
    list match {
      case Nil => false
      case head::tail =>
        if(head == element) {
          if(!belongsTo(tail, element)) true
          else false
        }else singleOccurrence(tail, element)
    }
    //    if(list.isEmpty) false
    //    else {
    //      if(list.head == element){
    //        if(!belongsTo(list.tail, element)) true
    //        else false
    //      }else singleOccurrence(list.tail, element)
    //    }
  }
  //11
  def biggerThan(list: List[Int], num: Int): List[Int] = {
    list match {
      case Nil => List()
      case head::tail =>
        if(head > num) head::biggerThan(tail, num)
        else biggerThan(tail, num)
    }
    //    if(list.isEmpty) List()
    //    else{
    //      if(list.head > num) list.head::biggerThan(list.tail, num)
    //      else biggerThan(list.tail, num)
    //    }
  }
  //12
  def concatenate(list1: List[Int], list2: List[Int]): List[Int] = list1 ++ list2
  //13
  def removeFistOccurrence(list: List[Int], num: Int): List[Int] = {
    list match {
      case Nil => List()
      case head::tail =>
        if(head == num) removeFistOccurrence(tail, num)
        else head::removeFistOccurrence(tail, num)
    }
    //    if(list.isEmpty) List()
    //    else{
    //      if(list.head == num) list.tail
    //      else list.head::removeFistOccurrence(list.tail, num)
    //    }
  }
  //14
  def removeLastElement(list: List[Int]): List[Int] = {
    list match {
      case head::Nil => List()
      case head::tail => head::removeLastElement(tail)
    }
    //    if(list.length == 1) List()
    //    else list.head::removeLastElement(list.tail)
  }
  //15
  def removeRepeated(list: List[Int]): List[Int] = {
    list match {
      case Nil => List()
      case head::tail =>
        if(belongsTo(tail, head)) head::removeRepeated(removeFistOccurrence(tail, head))
        else head::removeRepeated(tail)
    }
    //    if(list.isEmpty) list
    //    else{
    //      if(belongsTo(list.tail, list.head)) list.head::removeRepeated(removeFistOccurrence(list.tail, list.head))
    //      else list.head::removeRepeated(list.tail)
    //    }
  }
  //16
  def onTheList(list: List[Int], n: Int): List[Int] = {
    list match {
      case Nil => List()
      case head::tail =>
        if(head > n) head::onTheList(tail, n)
        else onTheList(tail, n)
    }
    //    if(list.isEmpty) List()
    //    else{
    //      if(list.head > n){
    //        list.head::onTheList(list.tail, n)
    //      }else onTheList(list.tail, n)
    //    }
  }
  //17
  def generateSequence(n: Int): List[Int] = {
    if(n == 1) List(1, -1)
    else generateSequence(n - 1) ++ List(n, -n)
  }
  //18
  def invert(list: List[Int]): List[Int] = {
    list match {
      case head::Nil => list
      case head::tail => invert(tail)++List(head)
    }
    //    if(list.length == 1) list
    //    else invert(list.tail) ++ List(list.head)
  }
  //19
  type TuplaList = (List[Int], List[Int])

  def shareForN(list: List[Int], n: Int): TuplaList = (list.take(n), list.drop(n))

  def getStart(list: List[Int], n: Int): List[Int] = {
    list match {
      case Nil => list
      case head::tail =>
        if(n == 0) List(head)
        else head::getStart(tail, n-1)
    }
    //    if(list.size == n || list.isEmpty) list
    //    else getStart(list.init, n)
  }

  def getEnd(list: List[Int], n: Int): List[Int] = {
    list match {
      case Nil => List()
      case head::tail =>
        if(n == 0) list
        else getEnd(tail, n - 1)
    }
    //    if(n == 0) list
    //    else getEnd(list.tail, n - 1)
  }

  def shareForN_2(list: List[Int], n: Int): TuplaList = {
    list match {
      case Nil => (List(), List())
      case head::tail =>
        if(n <= 0) (List(), list)
        else if(n > list.size) (list, List())
        else (getStart(list, n), getEnd(list, n))
    }
  }
  //20
  def intercalate(l1: List[Int], l2: List[Int]): List[Int] = {
    (l1, l2) match {
      case (Nil, head::tail) => l2
      case (head::tail, Nil) => l1
      case _ => l1.head::l2.head::intercalate(l1.tail, l2.tail)
    }
    //    if(l1.isEmpty) l2
    //    else if (l2.isEmpty) l1
    //    else{
    //      if(l1.length >= l2.length) List(l1.head) ++ List(l2.head) ++ intercalate(l1.tail, l2.tail)
    //      else List(l2.head) ++ List(l1.head) ++ intercalate(l1.tail, l2.tail)
    //    }
  }
  //21
  def unionNotRepeat(l1: List[Int], l2: List[Int]): List[Int] = {
    (l1, l2) match {
      case (Nil, head::tail) => l2
      case (head::tail, Nil) => l1
      case _ =>
        if(!l1.contains(l2.head)) l1 ++ List(l2.head) ++ unionNotRepeat(l1++List(l2.head), l2.tail)
        else l1 ++ unionNotRepeat(l1, l2.tail)
    }
    //    if(l1.isEmpty) l2
    //    else if(l2.isEmpty) l1
    //    else{
    //      if(!l1.contains(l2.head)) l1 ++ List(l2.head) ++ unionNotRepeat(l1 ++ List(l2.head), l2.tail)
    //      else l1 ++ unionNotRepeat(l1, l2.tail)
    //    }
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
  def intersection_2(l1: List[Int], l2: List[Int]): List[Int] = {
    (l1, l2) match {
      case (Nil, Nil) => List()
      case _ => removeRepeated(l1++l2)
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
    list match {
      case Nil => List(n)
      case head::tail =>
        if(head < n) head::insertTidy(tail, n)
        else n::head::tail
    }
    //    if(list.isEmpty) List(n)
    //    else{
    //      if(list.head < n) List(list.head) ++ insertTidy(list.tail, n)
    //      else List(n) ++ list
    //    }
  }
  //25
  def isOrdered(l: List[Int]): Boolean = {
    l match {
      case head::Nil => true
      case head::next::Nil =>
        if(head <= next) true else false
      case head::next::tail =>
        if(head >= next) false
        else{
          if(!isOrdered(tail)) true
          else false
        }
    }
    //    val sec = l.tail
    //    if(l.head >= sec.head) false
    //    else {
    //      if(!isOrdered(l.tail)) true
    //      else false
  }
  //26
  def order(l: List[Int]): List[Int] = {
    l match {
      case head::Nil => l
      case head::next::Nil => if(head < next) l else List(next, head)
      case head::next::tail => selectSmaller(l)::order(removeSelect(selectSmaller(l), l))
    }
    //    if(l.size == 1) l
    //    else{
    //      val smallerValue: Int = selectSmaller(l)
    //      List(smallerValue) ++ order(removeSelect(smallerValue, l))
    //    }
  }

  def removeSelect(i: Int, list: List[Int]): List[Int] = {
    list match {
      case Nil => List()
      case head::tail =>
        if(head == i) tail
        else head::removeSelect(i, tail)
    }
    //    if(list.head == i) list.tail
    //    else List(list.head) ++ removeSelect(i, list.tail)
  }

  def selectSmaller(list: List[Int]): Int = {
    list match {
      case head::Nil => head
      case head::next::Nil =>
        if(head < next) head else next
      case head::next::tail =>
        if(head < next) selectSmaller(head::tail)
        else selectSmaller(next::tail)
    }
    //    if(list.size == 1) list.head
    //    else{
    //      val rest: List[Int] = list.tail
    //      if(list.head < rest.head) selectSmaller(List(list.head) ++ rest.tail)
    //      else selectSmaller(rest)
    //    }
  }
  //27
  def toRotateLeft(l: List[Int], n: Int): List[Int] = {
    l match {
      case Nil => List()
      case head::tail =>
        if(n <= 0) l
        else toRotateLeft(tail:::head::Nil, n-1)
    }
    //    if(n <= 0) l
    //    else toRotateLeft(List(l.last) ++ l.init, n - 1)
  }
  //28
  def getLast(list: List[Int]): List[Int] = {
    list match {
      case Nil => Nil
      case head::Nil => head::Nil
      case head::next::Nil => next::Nil
      case head::next::tail => getLast(next::tail)
    }
  }
  def removeLast(list: List[Int]): List[Int] = {
    list match {
      case Nil => Nil
      case head::Nil => Nil
      case head::next::Nil => head::Nil
      case head::next::tail => head::removeLast(next::tail)
    }
  }
  def toRotateRigth(list: List[Int], n: Int): List[Int] = {
    list match {
      case Nil => List()
      case head::tail =>
        if(n <= 0) list else toRotateRigth(getLast(list)++removeLast(list), n - 1)
    }
    //    if(n <= 0) list
    //    else toRotateRigth(List(list.head) ++ list.tail, n - 1)
  }
  //29
  def strtoupper(x: String): String = {
    x match {
      case "" => ""
      case _ =>
        toupper(x.head) +: strtoupper(x.tail)
    }
    //    if(x.isEmpty) x
    //    else toupper(x.head) +: strtoupper(x.tail)
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
    str match {
      case "" => ""
      case _ =>
        if(str.length == 1) toupper(str.head).toString
        else {
          val _str: String = str.init
          if(_str.last == ' ') firstToupper(str.init) + toupper(str.last).toString
          else firstToupper(str.init) + lower(str.last).toString
        }
    }
  }
  //31
  def positions(string: String, pos: List[Int]): List[Char] = {
    if(pos.size == 1) List(walkString(string, pos.head - 1))
    else List(walkString(string, pos.head - 1)) ++ positions(string, pos.tail)
  }

  def walkString(str: String, i: Int): Char = {
    str match {
      case "" => ' '
      case _ =>
        if(i == 0) str.head else walkString(str.tail, i-1)
    }
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
    list match {
      case Nil => Nil
      case head::tail =>
        val repeat: List[Int] = compact(head, list, 0)
        if(repeat.length == 1) repeat +: toCompact(tail)
        else repeat +: toCompact(listAfterCases(list, repeat.head))
    }
  }
  def compact(n: Int, l: List[Int], x: Int): List[Int] = {
    l match {
      case Nil => Nil
      case head::tail =>
        if(head == n) compact(n, tail, x+1)
        else {
          if(x == 0) Nil else List(n, x)
        }
    }
  }
  def listAfterCases(list: List[Int], n: Int): List[Int] = {
    list match {
      case Nil => Nil
      case head::tail =>
        if(n == 0) list else listAfterCases(tail, n-1)
    }
    //    if(n == 0) list
    //    else listAfterCases(list.tail, n-1)
  }
  //37
  def pair_odd(list: List[Int]): (List[Int], List[Int]) = {
    (num_pair(list), num_odd(list))
  }
  def num_pair(list: List[Int]): List[Int] = {
    if(list.isEmpty) List()
    else{
      if(list.head%2 == 0) List(list.head) ++ num_pair(list.tail)
      else num_pair(list.tail)
    }
  }
  def num_odd(list: List[Int]): List[Int] = {
    if(list.isEmpty) List()
    else{
      if(list.head%2 != 0) List(list.head) ++ num_odd(list.tail)
      else num_odd(list.tail)
    }
  }
  //38
  //  def isPerfectSquare(n: Int): Boolean = {
  //
  //  }
  //  def perfectSquare(n: Int): Int = {
  //    if(n%2 != 0)
  //  }
  //39
  def convertIntForBase(n: Int, b:Int): String = {
    if(n/b == 0) convertRest(n%b)
    else{
      convertIntForBase(n/b, b)+convertRest(n%b)
    }
  }
  def convertRest(n: Int): String = {
    if(n > 10)((n+55)toChar).toString
    else n.toString
  }
  //40
}