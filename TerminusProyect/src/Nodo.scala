class Nodo(val name : Int){
  
  var mapa =  Map[Any,Set[(Any,Any)]]()
  var pointers = List[(String, Int)]()
    
  def addToPointers(x: (String, Int)){
    pointers ::= x
  }
  
  def addMap(m : Map[Any, Set[(Any,Any)]]){
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