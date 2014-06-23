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

}