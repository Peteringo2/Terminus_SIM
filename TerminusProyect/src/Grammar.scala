import scala.io.Source._
import scala.util.control.Breaks

object Grammar {
	
	var Grammar:Map[String,List[String]] = Map()
	val token = ("@[a-zA-Z]+@").r
	val NonTerminal = ("<[a-zA-Z]+>").r
	var Firsts:Map[String,List[String]] = Map()
	var Follows:Map[String,Set[String]] = Map()
	
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
	
	
	def getFirsts()={
	  var first:Map[String,List[String]] = Map()
	  var Terminal: List[String] = List()
	  
	  Grammar.keys.foreach{ i => 
	    		Terminal = firstTree(i)
                 first +=(i -> Terminal)
            
	  }
	  Firsts = first
	  reFirst()
	  eleminateRepetedFirsts()
    }
	
	def eleminateRepetedFirsts()={
	  Firsts.keys.foreach{i =>
	    Firsts += (i -> Firsts(i).removeDuplicates)
	  }
	}
	
	def reFirst()={
		val searchLoop = new Breaks
		var Terminal: List[String] = List()
				
		Firsts.keys.foreach{i =>
			  if(Firsts(i).contains("!")){
				  searchLoop.breakable{
				  for (x <- Grammar(i)){
				    val nextProd = (NonTerminal findFirstIn x).mkString("")
				    var newProduction = x.replaceFirst(nextProd, "")
				    if(newProduction.charAt(0) == '<'){
					      val find = (NonTerminal findFirstIn newProduction).mkString("")
					      var temp = Firsts(i)
					      temp = temp.take(Terminal.indexOf("!")-1) ::: temp.takeRight(temp.indexOf("!")+1)
					      temp = temp ::: Firsts(find)
					      Firsts += (i -> temp)
				      
				    }else if(newProduction.charAt(0) == '@'){
					      val find = (token findFirstIn newProduction).mkString("")
					      var temp = Firsts(i)
					      temp = temp.take(Terminal.indexOf("!")-1) ::: temp.takeRight(temp.indexOf("!")+1)
					      temp = temp ::: List(find.replaceAll("@", ""))
					      Firsts += (i -> temp)
					      searchLoop.break
				      
				    }else{
				      searchLoop.break
				    }
				  }
				  }
			  }
	  	}  
	}
	
	def firstTree(i:String):List[String] ={
	  var Terminal: List[String] = List()
	  for (x <- Grammar(i)){
              if(x.charAt(0) == '<'){
                val find = (NonTerminal findFirstIn x).mkString("")
                Terminal = Terminal ::: firstTree(find) 
                
              } 
		  	  if(x.charAt(0) == '@'){
                 val find = (token findFirstIn x).mkString("")
                 Terminal = Terminal ::: List(find.replaceAll("@", ""))
                
              }
		  	  if(x.charAt(0) == '!'){
            	  Terminal = Terminal ::: List(x.charAt(0)+"")
              }
		  	 
       }     
	  return Terminal
	  
	}
	

	def getFollows():Map[String,Set[String]]={
	  	var follows:Map[String, Set[String]] = Map()//Mapa que contiene los sets follows
	  	var changes : Boolean = true //variable para saber si hubo cambios en el ciclo
	  	Grammar.keys.foreach(key => follows += (key -> Set() ) ) //iniciamos follow con todos los no terminales
	  	follows += ("<S>" -> Set("$")) // inicia el follow de <S> con el símbolo de $
	  	while(changes){
	  		changes = false
	  		follows.keys.foreach(prod =>
	  			Grammar(prod).foreach(x => 
	  				for(a : String <- NonTerminal findAllIn x){
	  					var i = countSubstring(x, a);
	  					var sub_index = 0;
	  					while(i > 0){
	  					i -= 1
	  					var index : Int = (x.indexOf(a, sub_index) + a.length)
	  					sub_index += a.length
	  					var set = follows(a)
	  					var new_set : Set[String] = Set()
	  					if(index >= x.length)
	  						new_set = set.union(follows(prod))
	  					else if(x(index) == '@'){
	  						//agregamos terminal
	  					    new_set = Set((token findFirstIn x.substring(index)).mkString("").replaceAll("@", ""))
	  					}
	  					else if(x(index) == '<'){
	  					    
	  						var beta = (NonTerminal findFirstIn x.substring(index)).mkString("")
   		 					new_set = set.union(Firsts(beta).toSet.filter(a =>  a != "!") )
   		 					var beta_index = index
   		 					while(Firsts(beta).contains("!") && beta_index < x.length){
   		 					  beta_index += beta.length
   		 					  if(beta_index >= x.length)
   		 					    new_set = new_set.union(follows(prod))
   		 					  else if(x(beta_index) == '<') {
   		 					    beta = (NonTerminal findFirstIn x.substring(beta_index)).mkString("")  
   		 					    new_set = new_set.union(Firsts(beta).toSet.filter(a => a != "!"))
   		 					  }
   		 					  else
   		 					    new_set = new_set.union(Set((token findFirstIn x.substring(beta_index)).mkString("").replaceAll("@", "")))	
   		 					}
	  						if(beta_index > x.length) new_set = new_set.union(follows(prod))
	  					}
	  					
	  					var flag = false
	  					new_set.foreach(x => if(!set.contains(x)) flag = true)
	  					
	  					if(flag){
    		 				var follow_a = follows(a)
    		 				follow_a = follow_a.union(new_set)
   		 					follows += (a -> follow_a)
   		 					changes = true
   		 				}
	  					}
	  				}
	  			)	
	  		)
	  		
	  	}
	  	Follows = follows
	  return follows
   }
	
	def countSubstring(str1:String, str2:String):Int={
	  def count(pos:Int, c:Int):Int={
      val idx=str1 indexOf(str2, pos)
      if(idx == -1) c else count(idx+str2.size, c+1)
   }
   count(0,0)
}
	
}