
class BottomUp{  
	
  val tokenator = ("@[a-zA-Z]+@|<[a-zA-Z]+>").r
  var nodes_names = 1
  var lista_nodos : Set[Nodo] = Set()
  
  def getClosure(charToClosure :String, start : Int): Map[Any, List[(Any,Any)]] = {	  
	  var map : Map[Any, List[(Any,Any)]] = Map()
	  map += (charToClosure -> List())
	  for(str_chain <- Grammar.Grammar(charToClosure))
	  {
	     if(str_chain != "!") {
			var indices = getStringsToProcess(str_chain, start)
		    for(index <- indices){
				var temp =  map(charToClosure); temp = temp ::: List((str_chain, index));
				if(index < str_chain.length){
					str_chain charAt index match{
					case ('@') => map += (charToClosure -> temp)
					case ('<') => {	
					  map ++= (getClosure(Grammar.NonTerminal.findFirstIn(str_chain.substring(index)).mkString(""), 0))
					  var prod = map(charToClosure)
					  prod = prod ::: temp
					  map += (charToClosure -> prod)

					  }
					case _ => List((1,1,1))
					}
				}
				else 
					map += (charToClosure -> temp)
			}	  
		}
	  }
  map
  }
  
  def getStringsToProcess(cadena : String, index : Int)= {
     var indexes = Set[Int]()//lista de indices a retornar
     if(index < cadena.length()){//entra si el indice es menor a la cadena
    	  	  var flag = cadena(index) == '<'//revisiar si es '<'
    	  	  var chain = tokenator.findFirstIn(cadena.substring(index)).mkString("")//obtenemos la cadena para ver si produce !
    	  	  indexes = indexes.union(Set(index))//añadimos el indice default a la lsita
			  while(flag){//si si es '<'
			      flag = false
				  if(Grammar.Grammar(chain).contains("!")){//si la key produce epsilon
					  indexes = indexes.union(Set(indexes.last + chain.length))//añadimos el nuevo indice a procesar a nuestro set
					  chain = tokenator.findFirstIn(cadena.substring(indexes.last)).mkString("")//obtenemos la siguiente cadena
					  if(indexes.last < cadena.length) flag = cadena(indexes.last) == '<'//revisamos si se sigue cumpliendo que x(0) == '<' para saber si regresar
				  }
			  }
	 }
     else 
       indexes = indexes.union(Set(index))
      if(cadena == "@END@<enter>") println(indexes)
	 indexes
  }

  def nodeClosureCreater(n : Nodo) : Set[Nodo] = {
			  for(key <- n.mapa.keys; lista_tuples <- n.mapa(key)){
				  if(lista_tuples._2.asInstanceOf[Int] < lista_tuples._1.toString.length){
					  var letra_movimiento = tokenator.findFirstIn(lista_tuples._1.toString.substring(lista_tuples._2.asInstanceOf[Int])).mkString("")
					  var start = (letra_movimiento.length() + lista_tuples._2.asInstanceOf[Int])
					  
			    	  if(start <= lista_tuples._1.toString.length()){
			    	  var nuevo_nodo = new Nodo(nodes_names)
					  
			    	  
			    	  if(n.name == 3) println("llave" + key + "index" + start)
					  nuevo_nodo.addMap(getClosure(key.toString, start))
					  
					  var name = equalsMaps(nuevo_nodo)
					  
					  if(name != -1) n.addToPointers((letra_movimiento, name))
					  else{ 
//					    println("sali del nodo: " + n.name + " llave " + key.toString + " movi: " + letra_movimiento)
//					  println("name: " + nuevo_nodo.name)
//					  println("metroid" + nuevo_nodo.mapa)
//					  println()
					    lista_nodos = lista_nodos.union(Set(nuevo_nodo))
					    nodes_names += 1
					    n.addToPointers((letra_movimiento, nuevo_nodo.name)) 
						var flag = nodoTermino(nuevo_nodo)
						if(flag){lista_nodos = lista_nodos.union(nodeClosureCreater(nuevo_nodo))}
					  }
			    	  
			    	  
			    	  }
				      
				  }
			  }

  lista_nodos
  }
  
  def equalsMaps(nodo : Nodo) : Int = {
	  for(n <- lista_nodos) if(n.mapa == nodo.mapa) return n.name
	  return -1
  }
  
  def nodoTermino(n : Nodo) : Boolean = {
    for(key <- n.mapa.keys; lista_tuples <- n.mapa(key))
    	if(lista_tuples._1.toString.length() < lista_tuples._2.asInstanceOf[Int] ) return false
    return true
  }


}