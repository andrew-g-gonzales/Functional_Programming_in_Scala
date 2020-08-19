package fpinscala.ch2.hof.polymorphic

object Sorted {

  def isSorted[A](as:Array[A], ordered: (A,A)=>Boolean):Boolean ={

    def loop(n:Int): Boolean = {
      if(n >= as.length-1){
        true
      }else if(!ordered(as(n-1),as(n))){
        false
      }else{
        loop(n+1)
      }
    }

    loop(1)
  }

}
