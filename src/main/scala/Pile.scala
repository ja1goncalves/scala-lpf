object Pile extends App {
  abstract class Pile {}
  case class NodePile(value: Int, next: Pile = Empty) extends Pile{}
  case object Empty extends Pile {}

  def push(n: Int, pile: Pile): Pile = {
    pile match {
      case Empty => NodePile(n, Empty)
      case NodePile(value, Empty) => NodePile(value, NodePile(n, Empty))
      case NodePile(value, next) => NodePile(n, pile)
    }
  }

  def pop(pile: Pile): Pile = {
    pile match {
      case Empty => Empty
      case NodePile(value, Empty) => Empty
      case NodePile(value, next) => next
    }
  }

  def top(pile: Pile): Int = {
    pile match {
      case Empty => -1
      case NodePile(value, Empty) => value
      case NodePile(value, next) => value
    }
  }

  def pileToList(pile: Pile): List[Int] = {
    pile match {
      case Empty => Nil
      case NodePile(value, Empty) => List(value)
      case NodePile(value, next) => value::pileToList(next)
    }
  }
}
