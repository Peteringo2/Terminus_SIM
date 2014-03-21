import scala.collection.mutable.Map

class Nodo(val name : Int){
  
  val mapa =  Map[Any,Any]()
  var pointers = List[Int]()
    
  def addToPointers(x: Int){
    pointers ::= x
  }
  
  def addListToMap(listToAdd: List[(Any,Any)]){
    
    for(l <- listToAdd)
      addToMap(l._1, l._2)
    
  }
  
  def addToMap(p :(_,_)){
    this.mapa += p._1 -> p._2
  }
}