case class FunctionDeclaration(name:String, params: List[String], body: Expression)
case class Program(functionEnvironment: List[FunctionDeclaration], main:Expression)

sealed abstract class Expression
//TODO Add case classes to represent the Expression AST
case class ExpCond(predicate: Expression, expression1: Expression, expression2: Expression) extends Expression
case class ExpPredicate(identifier : String, params : List[Expression]) extends Expression
case class ExpFunction(identifier: String, params : List[Expression]) extends Expression
case class ExpInteger(digit : Int) extends Expression
case class ExpLists(expression : List[Expression]) extends Expression
case class ExpVariable(identifier : String) extends Expression
case class ExpThrowExp(exceptionId : String) extends Expression
case class ExpTryCatch(expression : Expression, handlers : List[Expression]) extends Expression
case class ExpHandler(exceptionId : String, expression : Expression) extends Expression

sealed abstract class Value
//TODO Add case classes to represent the Value AST
case class ValInteger(digits : Int) extends Value
case class ValVariable(identifier : String) extends Value
case class ValList(list : List[Value]) extends Value
case class ValPredicate(predicate : Boolean) extends Value
case class ValThrowExp(exceptionId : String) extends Value
case class ValHandler(exceptionId : String, value : Value) extends Value

// case class ExpIdentifier(param: String) extends Value
// represtieren nichts im syntaxtree, weil subparsers

object PrettyPrinter {

  //TODO This method should produce formatted output for the output values
  def print(value: Value):String = value match {
    case ValInteger(a : Int) => printInteger(a)
    case ValList(value : List[Value]) =>
      value match {
        case List(ValThrowExp(a), _) => printException(a)
        case List(_, ValThrowExp(a)) => printException(a)
        case _ => printList(value)
      }
    case ValThrowExp(ex : String) => printException(ex)
    case ValHandler(expId, value) => print(value)

    case _ => value.toString
  }

  //An Integer is printed as the Integer itself e.g. 42
  def printInteger(i:Int):String = {
    i.toString
  }

  //A List is printed as the printed Elements separated by a colon and enclosed with square brackets e.g. [42,[],1]
  //  Scala's mkString function might be helpful to do this.
  def printList[T](list: List[T]): String = list.map {
    case a: List[_] => printList(a)
    case ValInteger(a) => printInteger(a)
    case a          => a
  } mkString("[", ",", "]")

  //A Exception Value is printed as the String "Uncaught exception $ex!", where $ex is replaced by the name of the exception
  //  e.g. Uncaught exception DivByZero!
  def printException(e:String):String = {
    "Uncaught exception "  + e + "!"
  }
}
