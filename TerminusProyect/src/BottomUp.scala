
class BottomUp{  
	
	
  val tokenator = ("@[a-zA-Z]+@|<[a-zA-Z]+>").r
  var nodes_names = 1
  var lista_nodos : Set[Nodo] = Set()
  var listOfRoot = List[(String, String)]()
  var states = List[Int]()
  var symbols = List[String]()
  var tablaSLR = Map[(Int,String), String]()
  var entrada = List[String]()
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
    	  	  indexes = indexes.union(Set(index))//a��adimos el indice default a la lsita
			  while(flag){//si si es '<'
			      flag = false
				  if(Grammar.Grammar(chain).contains("!")){//si la key produce epsilon
					  indexes = indexes.union(Set(indexes.last + chain.length))//a��adimos el nuevo indice a procesar a nuestro set
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
//					  println("sali del nodo: " + n.name + " llave " + key.toString + " movi: " + letra_movimiento)
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
  
  
	def getTerminalAndNonTerminalForSLRTable{

		for( n <- lista_nodos; apuntador <-  n.pointers){
			tablaSLR += (n.name, apuntador._1) -> ("S" + apuntador._2.toString)
		}
		tablaSLR += (1, "$") -> ("ACCEPT")
		tablaSLR += (0, "<S>") -> ("S1")
	}

	def getReduceForSLRTable{
	  
	  for(x <- listOfRoot) println(x)
	  
		for(n <- lista_nodos; key  <- n.mapa.keys ; prodThis <- n.mapa(key) if prodThis._1.toString.length <= prodThis._2.asInstanceOf[Int] ; 
			follow <- Grammar.Follows(key toString) ; lineIndex <- 0 until listOfRoot.length if listOfRoot(lineIndex) == (key toString, prodThis._1.toString)){
		  if(follow != "$")
			  tablaSLR += (n.name, "@"+follow+"@") -> ("R"+ lineIndex.toString)
			else
				tablaSLR += (n.name, follow) -> ("R"+ lineIndex.toString)
		} 
				tablaSLR += (1, "$") -> ("ACCEPT")
		tablaSLR += (0, "<S>") -> ("S1")
	}	

	def SLRParse{
	  var flag = false
			  for(x <- Lex.tokens) entrada :+= "@" + x(0) + "@"
			  states ::= 0
				entrada = this.entrada ::: List("$")
				//for(z <- entrada) println("zora "+z)
				while(!flag){
					var key = (states.head, entrada.head)
							println("pila de estados --- " + states + " simbolos --- " + symbols + " entrada --- " + entrada)
							println("tope stack: " + states.head + "top entrada: " + entrada.head)
							if(tablaSLR.keys.exists(_==key)){
								tablaSLR(key).charAt(0) match {
								case 'S' => {
											states ::= tablaSLR(key).substring(1).toInt
											symbols ::= entrada.head
											entrada = entrada.tail
								}
								case 'R' => {
//											val sizeOfProd = listOfRoot(tablaSLR(key).substring(1).toInt)._2.length									
											var produccion = listOfRoot(tablaSLR(key).substring(1).toInt)
											var num_pop = simbolsToPop(produccion._2, symbols)
											println("num_pop: " + num_pop)
											for(symbol <- 0 until num_pop){
												states = states.tail
														symbols = symbols.tail
											}
											
											symbols ::= listOfRoot(tablaSLR(key).substring(1).toInt)._1
											//println("pit" + (states.head, symbols.head))
											states ::= tablaSLR((states.head, symbols.head)).substring(1).toInt
								
								}
								
								case 'A' => {
								  println("haz sido victorioso")
								  flag = true
								}
							}
							}
							else {
								println("I AM ERROR")
								flag = true
							}
				}
	}
	
	def simbolsToPop(c1 : String, c2 : List[String]) : Int = { 
//	    println("lista: " + c2)
//	    println("string: " + c1)
//	    println()
		var num_to_pop = 0 
		var num_arr = 0
		var num_< = 0
		for(x : Char <- c1.toList){
		  if(x == '<') num_< += 1
		  if(x == '@') num_arr += 1
		}

		var num_tokens = ((num_arr / 2) + num_<)
		
//		println("cadena: " + c1 + "cuantostokens" + num_tokens)

		while(num_tokens > 0){
		  var index = 0
		  var encounters = 0

		  var pasada = num_tokens - 1
		  //println("pasada " + pasada + "len " + c2.length)
		  if(pasada < c2.length){
		  while(index < c1.length){
			  var str_com = tokenator.findFirstIn(c1.substring(index)).mkString("")
			  //println("string: " + str_com + "ele at list: " + c2(pasada))
			  if(str_com == c2(pasada)){
					index += str_com.length
				    num_to_pop += 1
			  }
			  else{
			    if(str_com(0) == '<'){
			      if(Grammar.Grammar(str_com).contains("!")){
			        index += str_com.length
			        pasada += 1
			      }
			    }
			  }
			  pasada -= 1
			  if(pasada == -1) return num_to_pop
		  }

		  }

		  num_tokens -= 1
		}
		num_to_pop
	}

	def returnRootToList{
		for(key <- Grammar.Grammar.keys ; prod <- Grammar.Grammar(key))
				listOfRoot :+= (key.toString, prod)
		 
	}
}