class Nodo(val name : Int){
  
  var mapa =  Map[Any,Any]()
  var pointers = List[Int]()
    
  def addToPointers(x: Int){
    pointers ::= x
  }
  
  def addMap(m : Map[Any, List[(Any,Any)]]){
    mapa ++= m
  }
  
//  def addListToMap(listToAdd: List[(Any, List[(Any,Any)])]){
//    
//    for(l <- listToAdd)
//      addToMap(l._1, l._2)
//    
//  }
//  
//  def addToMap(key : Any, p : List[(_,_)]){
//    this.mapa += key -> p
//  }
}