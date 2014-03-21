import scala.io.Source._

object Grammar {
	
	var Grammar:Map[String,List[String]] = Map()
	val token = ("@[a-zA-Z]+@").r
	val NonTerminal = ("<[a-zA-Z]+>").r
	
	def generateGrammar(GrammarSource:String){
     val GrammarCodeLine = fromFile(GrammarSource).getLines
     var Prod: List[String] = List()
     
     for(x<-GrammarCodeLine){
    	 val production: Array[String] = x.split("->")
    	 if (Grammar.contains(production(0))){  //si ya existe la key
    		 Prod = Grammar(production(0)) 
    		 Prod = Prod ::: List(production(1))
    		 Grammar +=(production(0) -> Prod)
    	 }else{
    	   Prod = List(production(1))
    	   Grammar +=(production(0) -> Prod)  //si no existe la key
    	 } 
	 }
    }
	
	
	def getFirsts():Map[String,List[String]]={
	  var firsts:Map[String,List[String]] = Map()
	  
	  var Terminal: List[String] = List()
	  
	  Grammar.keys.foreach{ i => 
	    		Terminal = firstTree(i)
                 firsts +=(i -> Terminal)
            
	  }
	 return firsts 
    }
	
	def firstTree(i:String):List[String] ={
	  var Terminal: List[String] = List()
	  for (x <- Grammar(i)){
              if(x.charAt(0) == '<'){
                val find = (NonTerminal findFirstIn x).mkString("")
                Terminal = Terminal ::: firstTree(find)
                
              }else if(x.charAt(0) == '@'){
                 val find = (token findFirstIn x).mkString("")
                 Terminal = Terminal ::: List(find.replaceAll("@", ""))
                
              }else{
            	  Terminal = List(x.charAt(0)+"")
              }
       }     
	  return Terminal
	  
	}
	

//	def getFollows(GrammarSource:String):Map[String,List[String]]={
//	  	var follows:Map[String,List[String]] = Map()
//	  	var follows:Map[String, List[String]] = Map()//Mapa que contiene los sets follows
//	  	val changes : Boolean = true //variable para saber si hubo cambios en el ciclo
//	  	Grammar.keys.foreach(key => follows += (key -> List() ) ) //iniciamos follow con todos los no terminales
//	  	follows += ("S" -> List("$")) // inicia el follow de <S> con el símbolo de $
//
//	  	while(changes){
//	  		changes = false
//	  		follows.keys.foreach(prod =>
//	  			Grammar(prod).foreach(x => 
//	  				for(A : String <- NonTerminal findAllIn x){
//	  					index : Int = x(x.indexOfSlice(A) + A.length)
//	  					set = follows(A)
//	  					if(index >= x.length)
//	  						new_set = set ::: follows(x)
//	  					else if(x(index) == "@"){
//	  						//agregamos terminal
//	  					}
//	  					else if(x(index) == "<"){
//	  						val beta = (NonTerminal findFirstIn x.substring(index)).mkString("")
//    		 					new_set = set ::: firsts(beta).filter(a =>  a != "!")
//    		 					if(firsts(beta).contains("!"))
//    		 						new_set = set ::: follows(x)
//	  					}
//	  					if(set != new_set){
//    		 					follows(A) += new_set
//    		 					changes = true
//    		 				}
//	  				}
//	  			)	
//	  		)
//	  	}
//
//	  return follows
//    }
}