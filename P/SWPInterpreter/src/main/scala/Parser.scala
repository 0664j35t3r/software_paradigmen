import scala.util.parsing.combinator._

class ExpParser extends JavaTokenParsers {

  //Nonterminale
  val program: Parser[Program] = funcDeclarations ~ expression ^^
    {case fs~e => Program(fs,e)}

  private def funcDeclarations : Parser[List[FunctionDeclaration]] = "{" ~> repsep(funcDecl, ";") <~ "}"

  private def funcDecl : Parser[FunctionDeclaration] =
    identifier ~ "(" ~ repsep(identifier, ",") ~ ")" ~ "=" ~ expression ^^
      {case n~_~xs~_~_~e => FunctionDeclaration(n, xs, e)}

  //TODO Implement the Expression Parser and add additional parsers for terminal and non terminal symbols, where necessary
  private def expression : Parser[Expression] = cond | throwExp | function | tryCatch | list | int | variable

  private def cond : Parser[Expression] = "if" ~ predicate ~ "then" ~ expression ~ "else" ~ expression ^^
    {case _~pred~_~exp1~_~exp2 => ExpCond(pred, exp1, exp2)}

  private def function : Parser[Expression] = identifier ~ "(" ~ repsep(expression, ",") ~ ")" ^^
    {case id~_~es~_ => ExpFunction(id, es)}

  private def list : Parser[Expression] = "[" ~ "]" ^^
    {case _~_ => ExpLists(List())} | "[" ~ repsep(expression, ",") ~ "]" ^^
    {case _~lists~_ => ExpLists(lists)}

  private def variable : Parser[Expression] = identifier ^^
    {case id => ExpVariable(id)}

  private def throwExp : Parser[Expression] = "throw " ~ exceptionId ^^
    {case _~expId => ExpThrowExp(expId)}

  private def tryCatch : Parser[Expression] = "try " ~ expression ~ "catch" ~ "{" ~ repsep(handler, ";") ~ "}" ^^
    {case _~exp~_~_~handlers~_ => ExpTryCatch(exp, handlers)}

  private def handler : Parser[Expression] = (exceptionId | "_") ~ ":" ~ expression ^^
    {case id~_~exp => ExpHandler(id, exp)}

  //TODO Implement a Parser for Predicate
  private def predicate : Parser[Expression] = identifier ~ "?" ~ "(" ~ repsep(expression, ",") ~ ")" ^^
    {case id~_~_~e~_ =>  ExpPredicate(id+"?", e)}

  //Terminale
  private val int : Parser[Expression] = "[1-9][0-9]*|0|[1-9][0-9]*".r ^^
    {case integer => ExpInteger(integer.toInt)}

  private val identifier : Parser[String] = "[a-z][a-zA-Z0-9]*".r ^^
    {case id => id}

  private val exceptionId : Parser[String] = "[A-Z][a-zA-Z]*".r ^^
    {case expId => expId}
}

object ParseProgram extends ExpParser {
  def parse(s: String): ParseResult[Program] = {
    parseAll(program, s)
  }
}
