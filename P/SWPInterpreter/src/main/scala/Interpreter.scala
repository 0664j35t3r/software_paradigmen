object Interpreter {

  //Type aliases to make the signatures more readable
  type FunctionName = String
  type VariableName = String

  //TODO
  //This function should evaluate the given expression, using the functionEnvironment(delta)
  //and the variableEnvironment(omega).
  //
  //It might be helpful to define helper functions to evaluate different expressions,
  //like buildings or user defined functions.

  def interpret(fEnv: Map[FunctionName, FunctionDeclaration],
                vEnv: Map[VariableName, Value],
                expression: Expression): Value = expression match {
    case ExpInteger(a : Int) => ValInteger(a)
    case ExpFunction(identifier: String, params : List[Expression]) =>
      identifier match {
        case "plus" =>
          params.map(x => interpret(fEnv, vEnv, x)) match {
            case List(ValThrowExp(a), _) => ValThrowExp(a)
            case List(ValList(a), _) => ValThrowExp("TypeMismatch")
            case List(_, ValThrowExp(a)) => ValThrowExp(a)
            case List(_, ValList(b)) => ValThrowExp("TypeMismatch")
            case List(ValInteger(a), ValInteger(b)) => ValInteger(a + b)
            case _ => ValThrowExp("TypeMismatch")
          }
        case "minus" =>
          params.map(x => interpret(fEnv, vEnv, x)) match {
            case List(ValThrowExp(a), _) => ValThrowExp(a)
            case List(ValList(a), _) => ValThrowExp("TypeMismatch")
            case List(_, ValThrowExp(a)) => ValThrowExp(a)
            case List(_, ValList(b)) => ValThrowExp("TypeMismatch")
            case List(ValInteger(a), ValInteger(b)) => ValInteger(a - b)
            case _ => ValThrowExp("TypeMismatch")
          }
        case "mult" =>
          params.map(x => interpret(fEnv, vEnv, x)) match {
            case List(ValThrowExp(a), _) => ValThrowExp(a)
            case List(ValList(a), _) => ValThrowExp("TypeMismatch")
            case List(_, ValThrowExp(a)) => ValThrowExp(a)
            case List(_, ValList(b)) => ValThrowExp("TypeMismatch")
            case List(ValInteger(a), ValInteger(b)) => ValInteger(a * b)
            case _ => ValThrowExp("TypeMismatch")
          }
        case "div" =>
          params.map(x => interpret(fEnv, vEnv, x)) match {
            case List(ValThrowExp(a), _) => ValThrowExp(a)
            case List(ValList(a), _) => ValThrowExp("TypeMismatch")
            case List(_, ValThrowExp(a)) => ValThrowExp(a)
            case List(_, ValList(b)) => ValThrowExp("TypeMismatch")
            case List(ValInteger(a), ValInteger(b)) => if (b != 0) ValInteger(a / b) else ValThrowExp("DivByZero")
            case _ => ValThrowExp("TypeMismatch")
          }
        case "build" => params.map(x => interpret(fEnv, vEnv, x)) match {
          case List(ValThrowExp(a), _) => ValThrowExp(a)
          case List(_, ValThrowExp(a)) => ValThrowExp(a)
          case List(_, ValInteger(a)) => ValThrowExp("TypeMismatch")
          case List(ValList(a), ValList(b)) => ValList(a:::b)
          case List(ValInteger(a), ValList(b)) => ValList(ValInteger(a)::b)
          case _ => ValThrowExp("TypeMismatch")
        }
        case "first" => params.map(x => interpret(fEnv, vEnv, x)) match {
          case List(ValThrowExp(a)) => ValThrowExp(a)
          case List(ValList(a)) => if (a.isEmpty) ValThrowExp("EmptyList") else a.head
          case _ => ValThrowExp("TypeMismatch")
        }
        case "rest" => params.map(x => interpret(fEnv, vEnv, x)) match {
          case List(ValThrowExp(a)) => ValThrowExp(a)
          case List(ValList(a)) =>if (a.isEmpty) ValThrowExp("EmptyList") else ValList(a.tail)
          case _ => ValThrowExp("TypeMismatch")
        }
        case _ =>
          if (fEnv.keySet.contains(identifier)) {
            val fDec : FunctionDeclaration = fEnv(identifier)
            if (fDec.params.length != params.length) ValThrowExp("TypeMismatch")
            var varEnv : Map[VariableName, Value] = Map[VariableName, Value]()
            var len = 0
            while (len < fDec.params.length) {
              varEnv += ((fDec.params(len), interpret(fEnv, vEnv, params(len))))
              len += 1
            }
            interpret(fEnv, varEnv, fDec.body)
          }
          else ValThrowExp("TypeMissmatch")
      }
    case ExpCond(predicate : Expression, expression1 : Expression, expression2 : Expression) =>
      val value = interpret(fEnv, vEnv, predicate)
      value match {
        case ValPredicate(true) => interpret(fEnv, vEnv, expression1)
        case ValPredicate(false) => interpret(fEnv, vEnv, expression2)
        case _ => value
      }
    case ExpPredicate(identifier : String, params : List[Expression]) =>
      identifier match {
        case "eq?" =>
          params.map(x => interpret(fEnv, vEnv, x)) match {
            case List(ValThrowExp(a), _) => ValThrowExp(a)
            case List(_, ValThrowExp(a)) => ValThrowExp(a)
            case List(ValInteger(a), ValInteger(b)) => ValPredicate(a.equals(b))
            case List(ValList(a), ValList(b)) => ValPredicate(a.equals(b))
            case _ => ValThrowExp("TypeMismatch")
          }
        case "gt?" =>
          params.map(x => interpret(fEnv, vEnv, x)) match {
            case List(ValThrowExp(a), _) => ValThrowExp(a)
            case List(_, ValThrowExp(a)) => ValThrowExp(a)
            case List(ValInteger(a), ValInteger(b)) => ValPredicate(a > b)
            case _ => ValThrowExp("TypeMismatch")
          }
        case "lt?" =>
          params.map(x => interpret(fEnv, vEnv, x)) match {
            case List(ValThrowExp(a), _) => ValThrowExp(a)
            case List(_, ValThrowExp(a)) => ValThrowExp(a)
            case List(ValInteger(a), ValInteger(b)) => ValPredicate(a < b)
            case _ =>  ValThrowExp("TypeMismatch")
          }
        case "atom?" =>
          params.map(x => interpret(fEnv, vEnv, x)) match {
            case List(ValThrowExp(a)) => ValThrowExp(a)
            case List(ValInteger(a)) => ValPredicate(true)
            case _ => ValPredicate(false)
          }
      }
    case ExpLists(expression : List[Expression]) =>
      val list = expression.map(x => interpret(fEnv, vEnv, x))
      var loop = 0
      while (loop < list.length) {
        list(loop) match {
          case ValThrowExp(a) => return ValThrowExp(a)
          case _ => loop += 1
        }
      }
      ValList(list)
    case ExpVariable(identifier : String) =>
      vEnv(identifier)
    case ExpThrowExp(exceptionId : String) =>
      ValThrowExp(exceptionId)
    case ExpTryCatch(expression : Expression, handlers : List[Expression]) =>
      val value = interpret(fEnv, vEnv, expression)
      value match {
        case ValThrowExp(a) =>
          matchHandler(fEnv, vEnv, a, handlers)
        case _ => value
      }
    case ExpHandler(exceptionId : String, expression : Expression) =>
      ValHandler(exceptionId, interpret(fEnv, vEnv, expression))
    case _ => ???
  }

  // Find the appropriate handler, if defined
  def matchHandler(fEnv: Map[FunctionName, FunctionDeclaration],
                   vEnv: Map[VariableName, Value],
                   expId : String,
                   handlers : List[Expression]) : Value = {
    var loop = 0
    var thereIsDefault = false
    // check, if handler is defined, if not, use the default, if defined
    while (loop < handlers.length) {
      val value = interpret(fEnv, vEnv, handlers(loop))
      value match {
        case ValHandler(a, valueHandler) =>
          if (a.equals("_")) thereIsDefault = true
          if (expId.equals(a)) return value else loop += 1
        case _ => return ValThrowExp("NothingWorks")
      }
    }
    if (thereIsDefault) {
      loop = 0
      while (loop < handlers.length) {
        val value = interpret(fEnv, vEnv, handlers(loop))
        value match {
          case ValHandler(a, valueHandler) =>
            if (a.equals("_")) return value else loop += 1
          case _ => return ValThrowExp("NothingWorks")
        }
      }
    }
    ValThrowExp("NothingWorks")
  }
}
