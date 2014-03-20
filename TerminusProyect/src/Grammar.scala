import scala.io.Source._

object Grammar {
	
	var Grammar:Map[String,List[String]] = Map()
	
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
	  val token = ("@[a-zA-Z]+@").r
	  val NonTerminal = ("<[a-zA-Z]+>").r
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
	
	
	def getFollows(GrammarSource:String):Map[String,List[String]]={
	  var follows:Map[String,List[String]] = Map()
	  return follows
    }
}