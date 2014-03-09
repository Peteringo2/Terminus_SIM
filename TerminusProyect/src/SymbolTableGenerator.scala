object SymbolTableGenerator {
  
	var SymbolTable:Map[Int,String] = Map()
	var CurrentKey = 0
	var CurrentFuntion = "univ"
	  
	def generate(Tuple:List[String],isDef:Boolean):List[String] ={
		var newTuple = Tuple
		val Class = Tuple(0)
		var Value = Tuple(1)
		var storekey = 0
		
		if(isDef == true)
		  CurrentFuntion = Value
		if(Class.equals("INIT"))
		  CurrentFuntion = "univ"
	
		if(Class=="ID"||Class=="OPERATOR"||Class=="COMPARATOR"||Class=="STRINGVALUE"||Class=="FLOATVALUE"||Class=="NUMVALUE"){
		  //Si hay que meterlo a la tabla
		  Value = CurrentFuntion+"."+Value
		  storekey = exist(Value)
		  if(storekey == -1){  //si no existe se agrega en la tabla:
			  SymbolTable +=((CurrentKey -> Value))
			  newTuple = remove(Value,newTuple)
			  newTuple = newTuple ::: List(CurrentKey+"")
			  CurrentKey+=1
		  }else{
		    SymbolTable +=((storekey -> Value))
		    newTuple = remove(Value,newTuple)
			newTuple = newTuple ::: List(storekey+"")
			return newTuple
		  }
		    //checar si ya existe en la tabla funcion Exist (Si regresa -1 significa que no existe.)
		}
		
		return newTuple
	  	
    }
	
	def remove(value: String, list: List[String]) = list diff List(value)
	
	def exist(Value:String): Int ={
	  	SymbolTable.keys.foreach{ i => 
            if(SymbolTable(i).equals(Value)){
              return i
            }else
            	 return -1
		}
	  return -1
	}

}