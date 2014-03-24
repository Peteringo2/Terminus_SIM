object TopDown {
	var Table:Map[Tuple2[String,String],String] = Map()
	var Pila:List[String]= List()

	def generateTable()={
		Grammar.Grammar.keys.foreach{ i => 
			for(j <- Grammar.Firsts(i)){
				Table += (Tuple2(i,j) -> SearchTree(i,j))
			}
		}
	}
	
	def SearchLL():Boolean={
	  var input = Lex.tokens
	  Pila = Pila.::("<S>")
	  
	  while(!input.isEmpty){
	    if(Pila.head.equals(input.head.head)){
	      Pila = Pila.tail
	      input = input.tail
	    }else{
	      val Production = Table(Tuple2(Pila.head,input.head.head))
	    		  if(Production==null) return false
	      Pila = Pila.tail
	      Pila.::(Production)
	    }

	  }
	  return true
	}


	def SearchTree(Key:String,Lookingfor:String):String ={
		val token = ("@[a-zA-Z]+@").r
				val NonTerminal = ("<[a-zA-Z]+>").r
				for (x <- Grammar.Grammar(Key)){
					if(x.charAt(0) == '<'){
						val find = (NonTerminal findFirstIn x).mkString("")
								if (!SearchTree(find,Lookingfor).isEmpty()){
									return x
								}
					}
	
					if(x.charAt(0) == '@'){
						val find = (token findFirstIn x).mkString("")
								if(Lookingfor.equals(find.replaceAll("@", ""))) return Lookingfor
	
					}else{
	
						//AQUI VA EL FOLLOW!
					}
				}     
		return ""
	
	}

}