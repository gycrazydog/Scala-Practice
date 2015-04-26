package firstScala
object sixToTen {
    def isPalindrome[A](L1 : List[A]) : Boolean = 
      if(L1.head==L1.last && L1.length>1) isPalindrome(L1.init.tail) 
      else if(L1.head==L1.last) true
      else false
    def flattenList[A](L1 : List[A]) : List[A] = L1 flatMap{
        case ms : List[A] => flattenList(ms)
        case e => List(e)
     }
    def removeDuplicate[A](L1 : List[A]) : List[A] = 
      L1.foldLeft(List[A]())((r,x)=>if(r.length>0&&x==r.last) r else r:+x)
    def encodeDuplicate[A](L1 : List[A]) : List[(A,Int)] = {
        if(L1.isEmpty) List()
        else{
            val (first,tail) = L1 span { _==L1.head }
            if(tail==Nil) List((first.head,first.length))
            else (first.head,first.length)::encodeDuplicate(tail)
        }
    }
    def eitherEncodeDuplicate[A](L1 : List[A]) : List[Either[A,(A,Int)]] = {
        encodeDuplicate(L1) map {
          t => if(t._2>1) Right(t) else Left(t._1)
        }
    }
    def main(args : Array[String]){
        println(isPalindrome(List(1,2,1)))
        println(flattenList(List(List(1, 1), 2, List(3, List(5, 8)))))
        println(eitherEncodeDuplicate(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    }
}