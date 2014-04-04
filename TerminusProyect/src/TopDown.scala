import scala.util.control.Breaks
/*
 * Genera la tabla LL(1) 
 */

object TopDown {
	var Table:Map[Tuple2[String,String],List[String]] = Map()
			var Pila:List[String]= List()
			var input: List[List[String]] = List()
			var nivel = 0
			var key = ""
			var f = ""

			def generateTable()={					//GENERA LA TABLA
				Grammar.Grammar.keys.foreach{ i =>
				for(j <- Grammar.Firsts(i)){
					nivel = 0
					SearchTree(i,j)
					
				}
				}
				addSyncs
			}

			def addSyncs()={					//AGREGA SYNCS A LA TABLA PARA EL MODO DE PANICO
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




			def SearchTree(Key:String,Lookingfor:String):Boolean ={  //METODO PARA AUXILIAR PARA LLENAR TABLA LL(1)
				if (nivel ==0)key = Key
				val token = ("@[a-zA-Z]+@").r
				val NonTerminal = ("<[a-zA-Z]+>").r
				
				if(Lookingfor == "!"){
					for(j <- Grammar.Follows(Key)){
						addToTable(Tuple2(Key,j), List("!"))
					}
				}else{
				
					for(x <- Grammar.Grammar(Key)){		//BUSCA COMO ARBOL DE PROFUNDIDAD SOBRE CADA FIRST QUE PRODUCCION ES LA NECESARIA
						if(nivel == 0) f = x
						if(x.charAt(0) == '<'){		//SI ES NO TERMINAL BAJA DE NIVEL EN EL ARBOL
							val find = (NonTerminal findFirstIn x).mkString("")
							nivel = nivel+1
							SearchTree(find, Lookingfor) 
							nivel = nivel-1
						} 
					  	if(x.charAt(0) == '@'){	//SI ES TERMINAL CHECA SI ES EL QUE ESTAMOS BUSCANDO
					  	  
						  	  val find = (token findFirstIn x).mkString("")
						  	  if(Lookingfor.equals(find.replaceAll("@", ""))){
						  		  addToTable(Tuple2(key,Lookingfor), List(f))
						  	  }
					  	}
					  	if(x.charAt(0) == '!'){
								addToTable(Tuple2(key,Lookingfor), List(f))
					  	}
					}
				
				}
				return false
			}

			def addToTable(element: Tuple2[String,String],newProductor: List[String]) ={	//METODO AUXLIAR PARA AGREGAR A LA TABLA LL(1)
				var temp = List[String]()
						if(Table.keys.exists((y)=>element == y)){
							temp = Table(element)
									temp = temp ::: newProductor
									Table += (element -> temp)
						}else{
							Table += (element -> newProductor)
						}
			}


			def SearchLL():Boolean={				//METODO DE ANALISIS POR LL(1)
			  println("Analizando mediante LL(1)...")
					input = Lex.tokens
					input = input ::: List(List("$"))
					Pila = Pila.::("$")
					Pila = Pila.::("<S>")
					var Line = 0
					var elem = 0

					while(!input.isEmpty){
					//println("\n \n Pila tiene" + Pila )  // <----------Descomentar para ver el proceso de en analisis LL(1)
					//println("Input tiene" + input )//<--------Descomentar para ver el proceso de en analisis LL(1)
					
						if(Pila.head.equals(input.head.head.replace("@", ""))){  //coincidence
							if(input.head.head.equals("ENTER")){ 
							  Line = Line + 1 
							  elem = 0
							  }
							//println("coincidencia en " + input.head.head) <--------Descomentar para ver el proceso de en analisis LL(1)
							Pila = Pila.tail
							input = input.tail
							elem = elem + 1
						}else{
							if(!(Pila.head.charAt(0) == '<')){
							  ErrorLog.writeLog(2, "error en match, no se encontró: " + Pila.head + " en el elemento " + elem , Line)
							  Pila = Pila.tail
							  if(Pila.head.equals("$")) return true
							}else{
							try{
								val Production = Table(Tuple2(Pila.head,input.head.head))
								if(Production==null) return false
								if(Production.head.equals("sync")){
									ErrorLog.writeLog(2, "no se esperaba "+ input.head.tail(0) + " en el elemento " + elem, Line)
									PanicMode.RestoreSync
									//println("quedo en pila " + Pila +"\n" + "quedo en input" + input )
								}else if(Production.head.equals("!")){
									Pila = Pila.tail
									if(Pila.head.equals("$")) return true
								}else{
									Pila = Pila.tail
											Pila = addToStack(Production(0))::: Pila
								}
							}catch{
							case _ => {
								if(input.head.head.equals("ENTER")) { //////SI EL IMPUT ES ENTER AGREGAR A LA TABLA EL MAP DE ENTER CON EL TERMINAL
									if(Pila.head.equals("$"))input = input.tail
									else if(Grammar.Firsts(Pila.head).contains("!")){
										Table+=(Tuple2(Pila.head,input.head.head)-> List("!"))
									}

								}else if(Pila.head.equals("<enter>") ||Pila.head.equals("<specialenters>")||Pila.head.equals("<specialenters>") ){
								  Pila = Pila.tail
								  if(Pila.head.equals("$")) return true
								}else{ 
									ErrorLog.writeLog(2, "no se esperaba "+ input.head.tail(0) + " en el elemento " + elem, Line)
									PanicMode.RestoreEmpty
								}
							}
							}
						}
						}

					}
					if(input.isEmpty && Pila.isEmpty){
						return true
					}
					return true
			}



			def addToStack(Production:String):List[String]={ //FUNCION AUXILIAR PARA IR ALIMENTANDO LA PILA
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