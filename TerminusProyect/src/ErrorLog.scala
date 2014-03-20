import java.io.PrintWriter

object ErrorLog {
  val S = new PrintWriter("ErrorLog.txt")
  val ErrorDic:Map[Int,String] = Map(1->"Lexical",2-> "Grammar",3->"Semantic")
  
  def writeLog(Type:Int, Error:String, Line:Int){
    	S.println(ErrorDic(Type)+" Error on: " + Error + " On line: " + Line)
	}

}