
class BottomUp{  
	
  val tokenator = ("@[a-zA-Z]+@|<[a-zA-Z]+>").r
  var nodes_names = 1
  var lista_nodos : Set[Nodo] = Set()
  var root = new Nodo(0)
  
  def getClosure(key : String, producciones : List[String], start : Int): Map[Any, Set[(String,Any)]] = {	  
	  var map : Map[Any, Set[(String,Any)]] = Map()
	  map += (key -> Set())
	  for(str_chain <- producciones)
	  {
	     if(str_chain != "!") {
			var indices = getStringsToProcess(str_chain, start)
		    for(index <- indices){
				var temp =  map(key); temp = temp.union(Set((str_chain, index)));
				if(index < str_chain.length){
					str_chain charAt index match{
					case ('@') => map += (key -> temp)
					case ('<') => {	
					  var nonterminal = Grammar.NonTerminal.findFirstIn(str_chain.substring(index)).mkString("")
					  map ++= (getClosure(nonterminal, Grammar.Grammar(nonterminal), 0))
					  var prod = map(key)
					  prod = prod.union(temp)
					  map += (key -> prod)

					  }
					case _ => List((1,1,1))
					}
				}
				else 
					map += (key -> temp)
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
	 indexes
  }

  def nodeClosureCreater(n : Nodo) : Set[Nodo] = {
			  for(key <- n.mapa.keys; lista_tuples <- n.mapa(key)){
				  if(lista_tuples._2.asInstanceOf[Int] < lista_tuples._1.toString.length){
					  var letra_movimiento = tokenator.findFirstIn(lista_tuples._1.toString.substring(lista_tuples._2.asInstanceOf[Int])).mkString("")
					  var start = (letra_movimiento.length() + lista_tuples._2.asInstanceOf[Int])
					  
			    	  if(start <= lista_tuples._1.toString.length()){
			    	  var nuevo_nodo = new Nodo(nodes_names)
			    	  
					  nuevo_nodo.addMap(getClosure(key.toString, List(lista_tuples._1), start))
					  
					  var name = equalsMaps(nuevo_nodo)
					  
					  if(name != -1) n.addToPointers((letra_movimiento, name))
					  else{ 
					  println("sali del nodo: " + n.name + " llave " + key.toString + " movi: " + letra_movimiento)
					  println("name: " + nuevo_nodo.name)
					  println("metroid" + nuevo_nodo.mapa)
					  println()
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