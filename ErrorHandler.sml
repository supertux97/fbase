use "util.sml";
use "operators.sml";
use "ERROR_HANDLER.sml";

(*Responsible for raising exceptions and warnings when extaordinary or unwanted events occur*)
structure ErrorHandler:ERROR_HANDLER = 
  struct
    datatype severity = MILD | MODERATE

    exception DivisionByZeroException of string
    exception UnexpectedSymbolException of string
    exception NoSuchSymbolException of string
    exception AritmetricException of string

    fun warning(degree:severity, explanation:string, lineNo:int):unit = 
      let val typeWarning = 
            case degree of 
              MILD => "Mild"
             |MODERATE => "Moderate"
      in
        Util.format("Warning[$]: $ at line no. $",[typeWarning,explanation,Util.$(lineNo)]) |> 
          Util.println
      end

    fun divisionByZero(context:string) = 
      raise DivisionByZeroException(
        Util.format("ERROR(Division by zero): Cannot divide by zero in expression: \"$\"", [context] ))

    fun noSuchSymbol(symbol:string, lineNo:int) = 
     raise NoSuchSymbolException(Util.format("ERROR(NoSuchSymbol): The symbol $ does not exist at line no. $", 
      [symbol, Util.$(lineNo)]))

    fun noSuchSymbolExpr(symbol:string, expr:string) = 
     raise NoSuchSymbolException(Util.format(
     "ERROR(NoSuchSymbol): The symbol $ does not exist in expression \"$\"", 
      [symbol, expr]))

    fun unexpectedSymbol(expected:string, found:string, context:string):exn= 
     raise UnexpectedSymbolException(
      Util.format("ERROR:(UnexpectedSymbol): Excptected $ but got $ in \"$\"",
      [expected, found, context]))
  end;
