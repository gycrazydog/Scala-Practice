package firstScala

object Test {
  def lastElement(L1: List[Int] ):Int = L1.last
  def lastSeconde(L1: List[Int]) = L1.init.last
  def lastNthRecursive[A](n: Int, ls: List[A]): A = {
    def lastNthR(count: Int, resultList: List[A], curList: List[A]): A =
      curList match {
        case Nil if count > 0 => throw new NoSuchElementException
        case Nil              => resultList.head
        case _ :: tail        =>
          lastNthR(count - 1,
                   if (count > 0) resultList else resultList.tail,
                   tail)
      }
    if (n <= 0) throw new IllegalArgumentException
    else lastNthR(n, ls, ls)
  }
  def firstN[A](n:Int,ls:List[A]): A = ls match {
    case _ :: tail => if (n == 1) ls.head else firstN(n-1,tail)
  }
  def main(args : Array[String]){
      println(lastElement(List(1,2)));
      println(firstN(2,List(1,2)))
  }
}