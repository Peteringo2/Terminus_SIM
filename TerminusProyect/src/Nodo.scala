import scala.collection.mutable.Map

class Nodo(val name : Int){
  
  val mapa =  Map[String, List[(String,Int)]]()
  var pointers = List[Int]()
  
  def addToPointers(x: Int){
    pointers ::= x
  }
  
  def addToMap(madd: Map[String, List[(String, Int)]] ){
    mapa ++= madd
  }
}