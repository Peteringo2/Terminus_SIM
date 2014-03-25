object TopDown {
	var Table:Map[Tuple2[String,String],List[String]] = Map()
	var Pila:List[String]= List()
	var nivel = 0

	def generateTable()={
		Grammar.Grammar.keys.foreach{ i => 
			for(j <- Grammar.Firsts(i)){
			  SearchTree(i,j)
			  nivel = 0
			}
		}
	}


	def SearchTree(Key:String,Lookingfor:String):Boolean ={
		val token = ("@[a-zA-Z]+@").r
		val NonTerminal = ("<[a-zA-Z]+>").r
				if(Lookingfor == "!"){
				  for(j <- Grammar.Follows(Key)){
					    //Table += (Tuple2(Key,j) -> List("!"))
					    addToTable(Tuple2(Key,j), List("!"))
				  }
				}else{
					for (x <- Grammar.Grammar(Key)){
						if(x.charAt(0) == '<'){
							val find = (NonTerminal findFirstIn x).mkString("")
									nivel = nivel + 1
								if(SearchTree(find,Lookingfor)){
									nivel = nivel-1
									if(nivel == 0){
									  //Table += (Tuple2(Key,Lookingfor) -> List(x))
									  addToTable(Tuple2(Key,Lookingfor), List(x))
									}else{
										return true
									}
								}
						}
		
						if(x.charAt(0) == '@'){
							val find = (token findFirstIn x).mkString("")
								if(Lookingfor.equals(find.replaceAll("@", ""))){
									  if(nivel == 0){
									    //Table += (Tuple2(Key,Lookingfor) -> List(x))
									    addToTable(Tuple2(Key,Lookingfor), List(x))
									  }else{
									    return true
									  }
								}
						}
					}
				}
		return false
	
	}
	
	def addToTable(element: Tuple2[String,String],newProductor: List[String]) ={
		var temp = List[String]()
		if(Table.keys.exists((y)=>element == y)){
			temp = Table(element)
			temp = temp ::: newProductor
			Table += (element -> temp)
		}else{
			Table += (element -> newProductor)
		}
	}
	
	
	def SearchLL():Boolean={
	  var input = Lex.tokens
	  input = input ::: List(List("$"))
	  Pila = Pila.::("$")
	  Pila = Pila.::("<S>")
	  
	  println("Pila inicial" + Pila)
	  println("Input inicial" + input)
	  
	  while(!input.isEmpty){
	    Thread sleep 3000
	    println("\n\n\n")
	    println("input: "+ input)
	    println("Pila: "+ Pila)
	    if(Pila.head.equals(input.head.head.replace("@", ""))){  //coincidence
	      println("coincidencia en " + input.head.head)
	      Pila = Pila.tail
	      input = input.tail
	    }else{
	      val Production = Table(Tuple2(Pila.head,input.head.head))
	    		  if(Production==null) return false
	    		  if(Production == '!'){
	    			  Pila = Pila.tail
	    		  }else{
				      Pila = Pila.tail
				      Pila = addToStack(Production(0))::: Pila
	    		  }
	    }

	  }
	  if(input.isEmpty && Pila.isEmpty){
	    println("la gramatica es correcta")
	    return true
	  }
	  return true
	}
	
	
	def addToStack(Production:String):List[String]={
	  var PilaTemp:List[String]= List()
	  var Cadena = Production
	  val token = ("@[a-zA-Z]+@").r
	  val NonTerminal = ("<[a-zA-Z]+>").r
	  while(!Cadena.isEmpty()){
		  if(Cadena.charAt(0) == '<'){
		     val find = (NonTerminal findFirstIn Cadena).mkString("")
		     PilaTemp = PilaTemp.::(find)
		     Cadena = Cadena.replace(find, "")
		  }else
		  if(Cadena.charAt(0) == '@'){
		    val find = (token findFirstIn Cadena).mkString("")
		    Cadena = Cadena.replace(find, "") 
		    PilaTemp = PilaTemp.::(find.replace("@", ""))
		  } else
		  if(Cadena.charAt(0) == '!'){
		    Cadena = Cadena.replace("!", "")
		  }
	  }
	  
	  return PilaTemp.reverse
	}

}