import java.io.PrintWriter
/*
 * Esta clase tiene solo la funcion write logg que dado una entrada de tipo y string de error, lo agrega a 
 * el archivo ErrorLog
 */
object ErrorLog {
  var Errors = 0
  val S = new PrintWriter("ErrorLog.txt")
  val ErrorDic:Map[Int,String] = Map(1->"Lexical",2-> "Grammar",3->"Semantic") //diccionario para saber que tipo de error es
  
  def writeLog(Type:Int, Error:String, Line:Int){
<<<<<<< HEAD
=======
	  	Errors = Errors + 1  //contador de errores
>>>>>>> 572baab
    	S.println(ErrorDic(Type)+" Error on: " + Error + " On line: " + Line)
	}

}