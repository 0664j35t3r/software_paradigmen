object SWPInterpreter {

  def main(args: Array[String]) {
  val Array(file) = args
  val source = scala.io.Source.fromFile(file)
  val fileString = source.mkString
  source.close()

  val result = evaluateProgram(fileString)

  Console.println("----------")
  Console.println(result)
  Console.println("----------")
  }

  //DO NOT CHANGE!
  //This function will be used to test your solution!
  def evaluateProgram(program:String):String = {
    val Program(funcs, exp) = ParseProgram.parse(program).get
    val result = Interpreter.interpret(funcs.map(x=>(x.name, x)).toMap, Map(), exp)
    PrettyPrinter.print(result)
  }

  //DO NOT CHANGE!
  //This function will be used to test your solution!
  def checkProgramGrammer(program:String):Boolean = {
    ParseProgram.parse(program).successful
  }
  
}
