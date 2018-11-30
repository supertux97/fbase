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
    exception MultipleTablesNoMergeException of string
    exception InvalidMergeException of string
    exception UnknownColumnException of string
    exception InvalidRefererException of string
    exception PipeFunctionTypeError of string
    exception PipeFunctionNotFound of string
    exception TypeErrorDefaultVal of string
    exception DataNotMatchingMetadataException of string
    exception NoDataFoundInDatafile of string

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
        Util.format("Cannot divide by zero in expression: '$'", [context] ))

    fun noSuchSymbol(symbol:string, lineNo:int) = 
     raise NoSuchSymbolException(Util.format("The symbol '$' does not exist at line no. $", 
      [symbol, Util.$(lineNo)]))

    fun noSuchSymbolExpr(symbol:string, expr:string) = 
     raise NoSuchSymbolException(Util.format(
     "The symbol '$' does not exist in expression '$'", 
      [symbol, expr]))

    fun unexpectedSymbolExpr(expected:string, found:string, context:string):exn= 
     raise UnexpectedSymbolException(
      Util.format("Excptected '$' but got '$' in '$'",
      [expected, found, context]))

    fun unexpectedSymbol(expected:string, found:string, lineNo:int):exn = 
     raise UnexpectedSymbolException(
      Util.format("Excptected '$' but got '$' at line no. $",
      [expected, found, Util.$(lineNo)]))

    fun malformedQuery(expected:string, found:string, lineNo:int):exn = 
      MalformedQueryException(Util.format("Expected '$' but got '$' at line no. $",
      [expected,found,Util.$(lineNo)] ))

    fun malformedMetadata(expected:string, found:string):exn = 
      raise MalformedQueryException(Util.format("Expected '$' but got '$' at line no. $",
      [expected,found] ))

    fun emptyQuery() = 
      raise EmptyQueryException("The query cannot be empty")

    fun typeErrorStoredData(
      expectedType:string,
      foundType:string,file:string,lineNo:int):exn  =
      raise TypeErrorStoredDataException(Util.format(
          "Expected data of type $ but found $ at line $ in datafile '$'", 
           [expectedType, foundType, Util.$(lineNo), file]))

   fun missingData(file:string, lineNo:int) = 
     raise MissingDataException(Util.format(
        "Missing data in file '$' at line no. $ No default data defined",
           [file, Util.$(lineNo)]))
 
  fun emptyDatafile(file:string) = 
    raise NoDataFoundInDatafile(Util.format("No data found in file: '$'", [file]))

  fun noSuchTable() = 
    raise  NoSuchTableException("One or more of the requested tables could not be found")

  fun unknownColumn(colname:string, tablename:string, lineNo:int) = 
   raise UnknownColumnException(
      Util.format("The column '$' in table '$' does not exist. At line no. $",
      [colname, tablename, Util.$(lineNo)])) 

  fun multipleTablesNoMerge(unit) = 
    raise MultipleTablesNoMergeException(
      "Cannot use multiple tables without using merge")
  
  fun invalidReferer(message:string) = 
    raise InvalidRefererException(message)
  fun invalidMerge(desc:string) = 
    raise InvalidMergeException(desc)

  fun pipeFunctionTypeError(foundType:string, requiredType:string) = 
    raise PipeFunctionTypeError(Util.format(
      "The function requires column to be of type $ but it is of type $", [foundType, requiredType]))

  fun pipeFunctionNotFound(name:string) = 
    raise PipeFunctionNotFound(Util.format(
    "There is no pipe-function with the  name of '$'", [name]))

  fun typeErrorDefaultVal(expected:string, found:string, 
    field:string, filename:string) = 
    raise TypeErrorDefaultVal(Util.format(
    "Expected default value to match type of data($) but it is not(is of type $) in field '$' in metadatafile '$'",
     [expected,found,field,filename]))

  fun dataNotMatchingMetadata(filename:string) = 
    raise DataNotMatchingMetadataException(
      Util.format("The data in $ have a structure different from the data in the corresponding metadatafile", 

       [filename]))
 
  fun printMsgAndExit(msg) = 
    (print("ERROR:" ^ msg);OS.Process.exit OS.Process.success)

  end;
