object PanicMode { 
	/*
	 * clase que recupera el error en panicMode de LL(1)
	 */
	def RestoreSync()={			//SI ENCUENTRA EN LA TABLA SYNC
		var recuperado=false
		TopDown.Pila = TopDown.Pila.tail	//SACA UN ELEMENTO DE LA PILA
		if(TopDown.Pila.head.equals("$")){		//CHECA QUE LA PILA NO QUEDE VACIA
		  TopDown.Pila = TopDown.Pila.::("<S>")		//SI QUEDO VACIA, LLENA CON EL INICIAL Y SACA DEL INPUT HASTA ENCONTRAR UN TERMINAL VALIDO
		  while(!TopDown.input.head.head.equals("$") && !recuperado){
		  	  try{
		  		  if(TopDown.Table(Tuple2("<S>",TopDown.input.head.head)).head.equals("sync")){ 
		  		    TopDown.input = TopDown.input.tail
		  		  }else{
		  		    recuperado = true
		  		  }
		  	
		  	  }catch{
		  	    case _ =>{
		  	    	TopDown.input = TopDown.input.tail
		  	    }
		  	      
		  	  }
		  }
		}
	}
	
	def RestoreEmpty()={		//SI NO ENCOTRO SYNC EN LA TABLA SACA UN OBJETO DEL INPUT
	  println("restore Empty")
		TopDown.input = TopDown.input.tail
	}

}