import scala.util.control.Breaks

object TopDown {
	var Table:Map[Tuple2[String,String],List[String]] = Map()
			var Pila:List[String]= List()
			var input: List[List[String]] = List()
			var nivel = 0
			var key = ""
			var f = ""

			def generateTable()={
				Grammar.Grammar.keys.foreach{ i =>
				//println("------en la KEY ----" + i)
				for(j <- Grammar.Firsts(i)){
					//println(j)
					SearchTree(i,j)
					nivel = 0
				}
				}
				addSyncs
			}

			def addSyncs()={
				Grammar.Grammar.keys.foreach{ i =>
				for(x <- Grammar.Follows(i)){
					try{
						Table(Tuple2(i,x))
					}catch{
					case _ =>	{Table += (Tuple2(i,x) -> List("sync"))}

					}
				}
				}
			}




			def SearchTree(Key:String,Lookingfor:String):Boolean ={
				val searchLoop = new Breaks
						// Thread sleep 3000
						if (nivel == 0) key = Key
						val token = ("@[a-zA-Z]+@").r
						val NonTerminal = ("<[a-zA-Z]+>").r
						if(Lookingfor == "!"){
							//println("encontramos vacion con la key: " + key)
							for(j <- Grammar.Follows(Key)){
								//Table += (Tuple2(Key,j) -> List("!"))
								addToTable(Tuple2(Key,j), List("!"))
							}
						}else{
							searchLoop.breakable{
								for (x <- Grammar.Grammar(Key)){
									if (nivel == 0) f = x
											if(x.charAt(0) == '<'){
												val find = (NonTerminal findFirstIn x).mkString("")
														nivel = nivel + 1
														if(SearchTree(find,Lookingfor)){
															nivel = nivel-1
																	if(nivel == 0){
																		//Table += (Tuple2(Key,Lookingfor) -> List(x))
																		addToTable(Tuple2(Key,Lookingfor), List(x))
																		searchLoop.break
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
														searchLoop.break
													}else{
														return true
													}
												}
									}
									if(x.charAt(0) == '!'){
										println("en " + key + " , " + Lookingfor + " vamos a agregar " + f)
										addToTable(Tuple2(key,Lookingfor), List(f))
										searchLoop.break
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
					input = Lex.tokens
							input = input ::: List(List("$"))
							Pila = Pila.::("$")
							Pila = Pila.::("<S>")
							var Line = 0

					println("Pila inicial" + Pila)
					println("Input inicial" + input)

					while(!input.isEmpty){

						if(Pila.head.equals(input.head.head.replace("@", ""))){  //coincidence
							if(input.head.head.equals("ENTER")){ Line = Line + 1}
							println("coincidencia en " + input.head.head)
							Pila = Pila.tail
							input = input.tail
						}else{
							try{
								val Production = Table(Tuple2(Pila.head,input.head.head))
								if(Production==null) return false
								if(Production.head.equals("sync")){
									ErrorLog.writeLog(2, "no se esperaba "+ input.head.head, Line)
									PanicMode.RestoreSync
									println("salimos")
									println("quedo en pila " + Pila +"\n" + "quedo en input" + input )
								}else if(Production.head.equals("!")){
									Pila = Pila.tail
								}else{
									Pila = Pila.tail
											Pila = addToStack(Production(0))::: Pila
								}
							}catch{
							case _ => {
								if(input.head.head.equals("ENTER")) { //////SI EL IMPUT ES ENTER AGREGAR A LA TABLA EL MAP DE ENTER CON EL TERMINAL
									if(Grammar.Firsts(Pila.head).contains("!")){
										Table+=(Tuple2(Pila.head,input.head.head)-> List("!"))
									}

								}else{ 
									ErrorLog.writeLog(2, ("no se esperaba" + Pila.head), Line)
									PanicMode.RestoreEmpty
								}
							}
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
											Cadena = Cadena.replaceFirst(find, "")
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