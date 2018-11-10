use "util/util.sml";
use "operators.sml";
use "ERROR_HANDLER.sig";

(*Responsible for raising exceptions and warnings when extaordinary or unwanted events occur*)
structure ErrorHandler:ERROR_HANDLER = 
  struct
    datatype severity = MILD | MODERATE

    exception DivisionByZeroException of string
    exception UnexpectedSymbolException of string
    exception NoSuchSymbolException of string
    exception AritmetricException of string
    exception EmptyQueryException of string
    exception MalformedQueryException of string
    exception MalformedMetadataException of string
    exception TypeErrorStoredDataException of string
    exception MissingDataException of string
    exception NoSuchTableException of string
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
        Util.format("ERROR: Cannot divide by zero in expression: \"$\"", [context] ))

    fun noSuchSymbol(symbol:string, lineNo:int) = 
     raise NoSuchSymbolException(Util.format("ERROR: The symbol $ does not exist at line no. $", 
      [symbol, Util.$(lineNo)]))

    fun noSuchSymbolExpr(symbol:string, expr:string) = 
     raise NoSuchSymbolException(Util.format(
     "ERROR: The symbol $ does not exist in expression \"$\"", 
      [symbol, expr]))

    fun unexpectedSymbolExpr(expected:string, found:string, context:string):exn= 
     raise UnexpectedSymbolException(
      Util.format("ERROR: Excptected $ but got $ in \"$\"",
      [expected, found, context]))

    fun unexpectedSymbol(expected:string, found:string, lineNo:int):exn = 
     raise UnexpectedSymbolException(
      Util.format("ERROR: Excptected $ but got $ at line no. $",
      [expected, found, Util.$(lineNo)]))

    fun malformedQuery(expected:string, found:string, lineNo:int):exn = 
      raise MalformedQueryException(Util.format("ERROR: expected $ but got$ at line no. $",
      [expected,found,Util.$(lineNo)] ))

    fun malformedMetadata(expected:string, found:string):exn = 
      raise MalformedQueryException(Util.format("ERROR: expected $ but got$ at line no. $",
      [expected,found] ))

    fun emptyQuery() = 
      raise EmptyQueryException("ERROR: The query cannot be empty")

    fun typeErrorStoredData(expectedType:string, foundType:string, lineNo:int, file:string):exn  =
      raise TypeErrorStoredDataException(Util.format(
          "ERROR: expected data of type $ but found $ at line $ in datafile $", 
           [expectedType, foundType, Util.$(lineNo), file]))

   fun missingData(file:string, lineNo:int) = 
     raise MissingDataException(Util.format(
        "ERROR:Missing data in line no. $ in file $. No default data defined",
           [file, Util.$(lineNo)]))
  
  fun noSuchTable(table:string) = 
    raise NoSuchTableException(Util.format("Data and/or metadata for table $ could not be found",[table]))
  end;
