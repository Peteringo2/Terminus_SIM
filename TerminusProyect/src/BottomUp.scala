
class BottomUp{  

	def Getclosure(charToClosure :String): List[_] = {
		
	  var s = ""
		val temp =
				for(cadena <- Grammar.Grammar(charToClosure))
					yield {
					cadena charAt 0 match{
					case ('@') => (charToClosure, (Grammar.token.findFirstIn(cadena).mkString("").substring(1, -1), 0))
					case ('<') => (charToClosure, (s = Grammar.token.findFirstIn(cadena).mkString("").substring(1, -1), 0))
					case _ => ()
					}
				}


	  val listToMap = s match{
	    case ("") => temp
	    case _ => temp ::: Getclosure(s)
	  }
	  
	  listToMap
	}
	
	def nodeCreater{
	  
	  val n = new Nodo(0)
	  
	  
	  
	}
	
}