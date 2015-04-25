package firstScala

object Test {
  def lastElement(L1: List[Int] ):Int = L1.init.last
  def main(args : Array[String]){
      println(lastElement(List(1,2)));
    
  }
}