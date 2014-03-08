import scala.util.matching.Regex
import scala.util.control._
import scala.io.Source._

object Lex{
	def analyze(F_SourceCode: String): List[List[String]] ={
	  
	  var TokenList: List[List[String]] = List()
	  val EndLine: List[List[String]] = List(List("ENTER"," "))
	  
	  var RegexList = scala.io.Source.fromFile("regex list.txt")
	  var Regexs = RegexList.mkString
	  
	  val SourceCodeLine = fromFile(F_SourceCode).getLines
	  var SourceCode = ""
	      
	  for(x<-SourceCodeLine){
		  SourceCode = format(x)
		  TokenList = TokenList ::: getTokens(SourceCode,"regex list.txt")
		  TokenList = TokenList ::: EndLine
		}
	  return TokenList
      
    }
	
	def getTokens(str : String, RegexFile: String):List[List[String]]={
	  val loop = new Breaks
	  var Line = str
      var Regexs = Regex
      var tokens: List[List[String]] = List()
      var words: Array[String] = Line.split(" ")
   
      for(i <- words){ 
		  var Rgx = ""
		  loop.breakable{
          val TokensLine = fromFile(RegexFile).getLines
		  for(x <- TokensLine){
			  var token: Array[String] = x.split(" ")
			  var Regex = (token(1)).r
			  
		  	  val find = (Regex findFirstIn i).mkString("")

		
			  if (!find.isEmpty()){
			    var tuple: List[String] = List(token(0),find)
				tuple = SymbolTableGenerator.generate(tuple) 
			    tokens = tokens ::: List(tuple)
				loop.break
			  }    
		  }
		  }
      }
	  return tokens
	}
	
	
	def format(SCode:String):String={
	  var SourceCode = SCode
	  SourceCode = SourceCode.replace(",", " , ")
	  SourceCode = SourceCode.replace(")", " ) ")
	  SourceCode = SourceCode.replace("(", " ( ")
	  SourceCode = SourceCode.replace("}", " } ")
	  SourceCode = SourceCode.replace("{", " { ")
	  SourceCode = SourceCode.replace(";", " ; ")
	  SourceCode = SourceCode.replace("=", " = ")
	  SourceCode = SourceCode.replace("+", " + ")
	  SourceCode = SourceCode.replace("-", " - ")
	  SourceCode = SourceCode.replace("*", " * ")
	  SourceCode = SourceCode.replace("/", " / ")
	  SourceCode = SourceCode.replace("%", " % ")
	  SourceCode = SourceCode.replace("*  *", "*")
	  SourceCode = SourceCode.replace("+  +", "+")
	  SourceCode = SourceCode.replace("-  -", "-")
	  SourceCode = SourceCode.replace("+  =", "+=")
	  SourceCode = SourceCode.replace("-  =", "-=")
	  SourceCode = SourceCode.replace("*  =", "*=")
	  SourceCode = SourceCode.replace("/  =", "/=")
	  
	  SourceCode = SourceCode.replace("=  =", "==")
	  SourceCode = SourceCode.replace("!  =", "!=")
	  SourceCode = SourceCode.replace("<", " < ")
	  SourceCode = SourceCode.replace(">", " > ")
	  SourceCode = SourceCode.replace("<  =", "<=")
	  SourceCode = SourceCode.replace(">  =", ">=")

	  return SourceCode
	} 
	
}