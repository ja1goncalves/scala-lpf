import Main.{Empty, Node, NodeBranch}

object Tree extends App {
  abstract class Node{}
  case class NodeBranch(value: Int, left:Node = Empty, right: Node = Empty) extends Node{}
  case object Empty extends Node {}

  def insertValueInNode(n: Int): NodeBranch = NodeBranch(n, Empty, Empty)

  def map(node: Node, f:Int => Int): Node = {
    node match {
      case Empty => Empty
      case NodeBranch(value, left, right) =>
        NodeBranch(f(value), map(left, f), map(right, f))
    }
  }

//  def highOrderLeftRight(tree: Node, f:Int => Int, g:Int => Int): Node = {
//    tree match{
//      case Empty => Empty
//      case NodeBranch(v,left,right) => NodeBranch(v ,map(left, f), map(right, g))
//    }
//  }

  def insertNode(node: Node, n: Int): Node = {
    node match {
      case Empty => NodeBranch(n)
      case NodeBranch(value, left, Empty) => NodeBranch(value, left, NodeBranch(n))
      case NodeBranch(value, Empty, right) => NodeBranch(value, NodeBranch(n), right)
      case NodeBranch(value, left, right) =>
        if(n > value) insertNode(right, n)
        else insertNode(left, n)
    }
  }

  def mirror(node: Node): Node = {
    node match {
      case Empty => Empty
      case NodeBranch(value, left, Empty) => NodeBranch(value, Empty, mirror(left))
      case NodeBranch(value, Empty, right) => NodeBranch(value, mirror(right), Empty)
      case NodeBranch(value, left, right) => NodeBranch(value, mirror(right), mirror(left))
    }
  }

  def sumTree(n1: Node, n2: Node): Node = {
    (n1, n2) match {
      case (Empty, Empty) => Empty
      case (Empty, n2) => n2
      case (n1, Empty) => n1
      case (NodeBranch(value1, left1, right1), NodeBranch(value2, left2, right2)) =>
        NodeBranch(value1+value2, sumTree(left1, left2), sumTree(right1, right2))
    }
  }

  def exist(n: Int, node: Node): Boolean = {
    node match {
      case Empty => false
      case NodeBranch(value, left, right) =>
        if(n > value) exist(n, right)
        else if (n < value) exist(n, left)
        else true
    }
  }

  def toList(node: Node): List[Int] = {
    node match {
      case Empty => List()
      case NodeBranch(value, left, right) =>
        toList(left):::List(value):::toList(right)
    }
  }

  def height(node: Node): Int = {
    node match {
      case Empty => -1
      case NodeBranch(value, left, right) =>
        if(height(left) > height(right)) 1 + height(left)
        else 1 + height(right)
    }
  }

  def sumNodes(node: Node): Int = {
    node match {
      case Empty => 0
      case NodeBranch(value, Empty, Empty) => value
      case NodeBranch(value, left, right) => sumNodes(left)+sumNodes(right)
    }
  }

  def prune(node: Node, n: Int): Node = {
    node match {
      case Empty => Empty
      case b:NodeBranch =>
        if(exist(n, node)){
          if(n == b.value) Empty
          else if(n > b.value) NodeBranch(b.value, b.left, prune(b.right, n))
          else NodeBranch(b.value, prune(b.left, n), b.right)
        }else node
    }
  }

  def nodeCount(node: Node): Int = {
    node match {
      case Empty => 0
      case b:NodeBranch => 1 + nodeCount(b.left) + nodeCount(b.right)
    }
  }

  def nodeAtLevel(tree:Node, lvl: Int): List[Node] = {
    tree match {
      case NodeBranch(v,l,r) => if(level(tree,v) == lvl) List(tree)
      else nodeAtLevel(l, lvl-1) ++ nodeAtLevel(r, lvl-1)
      case Empty => List()
    }
  }

  def level(node: Node, valueNode: Int): Int = {
    node match {
      case NodeBranch(v,l,r) => if(valueNode > v) 1 + level(r, valueNode)
      else if(valueNode < v) 1 + level(l, valueNode)
      else 1
      case Empty => 0
    }
  }

  def postOrder(node: Node): List[Int] = {
    node match {
      case Empty => Nil
      case b:NodeBranch => postOrder(b.left)++postOrder(b.right)++List(b.value)
    }
  }

  def preOrder(node: Node): List[Int] = {
    node match {
      case Empty => Nil
      case b:NodeBranch => List(b.value):::preOrder(b.left):::preOrder(b.right)
    }
  }

  //QuestaoPrimos
  //ArvoreDecisao
  //ArvoreHuffman
  case class NodeHuffman(v: Char = '*', l: Node = Empty, r: Node = Empty) extends Node {}

  def decode(str: String, node: NodeHuffman): String = {
    def aux_decode(str: String, original_node: NodeHuffman, node: NodeHuffman): String = {
      str match {
        case "" =>
          if(node.v != '*') node.v + ""
          else ""
        case _ =>
          if(node.v != '*') node.v + aux_decode(str, original_node, original_node)
          else if(str.head == '0') aux_decode(str.tail, original_node, node.l.asInstanceOf[NodeHuffman])
          else aux_decode(str.tail, original_node, node.r.asInstanceOf[NodeHuffman])
      }
    }
    aux_decode(str, node, node)
  }
  //ArvoreHeapSort

  //Convert
  def converter(s:Seq[Char]):Seq[Char]={
    s match{
      case head::tail =>
        if(head=='P')'T' +: converter(tail)
        else if(head=='O')'E' +: converter(tail)
        else if(head=='L')'N' +: converter(tail)
        else if(head=='A')'I' +: converter(tail)
        else 'S' +: converter(tail)

      case Nil => Nil
    }
  }
}
