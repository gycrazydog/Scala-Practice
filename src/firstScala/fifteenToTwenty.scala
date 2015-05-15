package firstScala

object fifteenToTwenty {
  def dropEveryN(N:Int,L1:List[Any]):List[Any] = {
    def dropN(c:Int,curList:List[Any]):List[Any] = (c,curList) match{
      case (_,Nil) => Nil
      case (1,h::tail) => dropN(N,tail)
      case (c,h::tail) => h::dropN(c-1,tail)
    }
    dropN(N,L1)
  }
  def sliceNK(N: Int,K: Int, L1:List[Any]) = L1.drop(N-1).take(K-N+1)
  def rotateList(N: Int,L1: List[Any]) = {
    val (l1,l2) = L1.splitAt((N+L1.length)%L1.length)
    l2:::l1
  }
  def removeAt(N: Int,L1: List[Any]) = List(L1.take(N-1):::L1.drop(N),L1(N))
  def main(args:Array[String]){
    println(dropEveryN(3,List(1,2,3,4,5,6,7,8)))
    println(sliceNK(3,5,List(1,2,3,4,5,6,7,8)))
    println(rotateList(-2,List(1,2,3,4,5,6,7,8,9)))
    println(removeAt(3,List(1,2,3,4,5,6,7,8,9)))
  }
}