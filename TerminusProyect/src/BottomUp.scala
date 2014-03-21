
class BottomUp{  

	def Getclosure(charToClosure :String): List[(Any,Any)] = {
		
	  var s = ""
		val temp =
				for(cadena <- Grammar.Grammar(charToClosure))
					yield {
					cadena charAt 0 match{
					case ('@') => (charToClosure, Grammar.token.findFirstIn(cadena).mkString("").substring(1, -1))
					case ('<') => (charToClosure, s = Grammar.token.findFirstIn(cadena).mkString("").substring(1, -1))
					case _ => (1,1)
					}
				}


	  val listToMap = s match{
	    case ("") => temp
	    case _ => temp ::: Getclosure(s)
	  }
	  
	  listToMap
	  
	}
	
	def nodeClosureCreater(prod : String, name : Int){
	  val n = new Nodo(name)
	  n.addListToMap(Getclosure(prod))
	}
	
	
}