import scala.annotation.tailrec

object scala99 {

  def last(list: List[Any]) = list.last

  def penultimate(list: List[Any]) = list.init.last

  def nth(index: Int, list: List[Any]) = list(index)

  def length(list: List[Any]) = list.length

  def reverse(list: List[Any]) = list.reverse

  def isPalindrome(list: List[Any]) = list == list.reverse

  def flatten(list: List[Any]): List[Any] = list match {
    case h :: t => h match {
      case l: List[_] => flatten(l) ::: flatten(t)
      case e => e :: flatten(t)
    }
    case Nil => Nil
  }

  def compress(list: List[Any]) = {
    def recursive(target: List[Any], before: Any): List[Any] = target match {
      case h :: t => if (h == before)
        recursive(t, h)
      else
        h :: recursive(t, h)
      case Nil => Nil
    }
    if (list.isEmpty) Nil
    else
      recursive(list, list.tail)
  }

  def pack(list: List[Any]) = {
    def recursive(target: List[Any], before: List[Any]): List[Any] = target match {
      case h :: t => if (h == before.head)
        recursive(t, h :: before)
      else
        before :: recursive(t, List(h))
      case Nil => List(before)
    }
    if (list.isEmpty) List(List())
    else
      recursive(list.tail, List(list.head))
  }

  def encode(list: List[Any]) = pack(list).collect {
    case a: List[_] => (a.length, a.head)
  }

  def encodeModified(list: List[Any]) = pack(list).collect {
    case a: List[_] => if (a.length == 1) a.head else (a.length, a.head)
  }

  def decode(list: List[(Int, Any)]) = {
    def unpack(packed: (Int, Any)): List[Any] = packed._1 match {
      case 0 => Nil
      case i => packed._2 :: unpack((i - 1, packed._2))
    }
    list.flatMap(unpack)
  }

  def encodeDirect(list: List[Any]) = {
    @tailrec
    def recursive(target: List[Any], packed: (Int, Any), ret: List[Any]): List[Any] = target match {
      case h :: t => if (h == packed._2)
        recursive(t, (packed._1 + 1, h), ret)
      else
        recursive(t, (1, h), ret :+ packed)
      case Nil => ret :+ packed
    }
    if (list.isEmpty) List(List())
    else
      recursive(list.tail, (1, list.head), List())
  }

}