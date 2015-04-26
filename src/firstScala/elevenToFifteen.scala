package firstScala

object elevenToFifteen {
  def decodeList[A](L1 : List[(Int,A)]):List[A] = L1 flatMap{
     e => List.fill(e._1)(e._2)
  }  
  def duplicateN[A](N : Int, L1 : List[A]) : List[A] = L1 flatMap{
     e => List.fill(N)(e) 
  }
  def main(args : Array[String]){
       println(duplicateN(3, List('a, 'b, 'c, 'c, 'd)))
  }
}