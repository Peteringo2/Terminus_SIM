object PanicMode { 
	
	def RestoreSync()={
		var recuperado=false
		println("entre a sync")
		TopDown.Pila = TopDown.Pila.tail
		if(TopDown.Pila.head.equals("$")){
		  println("si es primero")
		  TopDown.Pila = TopDown.Pila.::("<S>")
		  println(TopDown.Pila)
		  while(!TopDown.input.head.head.equals("$") && !recuperado){
			  println(" " + TopDown.input.head.head)
		  	  try{
		  		  println(TopDown.Table(Tuple2("<S>",TopDown.input.head.head)))
		  		  if(TopDown.Table(Tuple2("<S>",TopDown.input.head.head)).head.equals("sync")){ 
		  		    TopDown.input = TopDown.input.tail
		  		    println("estamos en " + TopDown.input )
		  		  }else{
		  		    println("recuperamos")
		  		    recuperado = true
		  		  }
		  	
		  	  }catch{
		  	    case _ =>{
		  	    	TopDown.input = TopDown.input.tail
		  	    }
		  	      
		  	  }
		  }
		}else{
		  TopDown.Pila = TopDown.Pila.tail
		}
	}
	
	def RestoreEmpty()={
		TopDown.input = TopDown.input.tail
	}

}