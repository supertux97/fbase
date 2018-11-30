fun main a = a;
signature ERROR_HANDLER = 
  sig
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
    datatype severity = MILD | MODERATE

    (*Severity*Explanation*)
    val warning:severity * string * int -> unit

    (*Context*)
    val divisionByZero: string-> exn

    (*Symbol*lineNo*)
    val noSuchSymbol: string * int -> exn

    (*Symbol*expression*)
    val noSuchSymbolExpr: string * string -> exn

    (*Expected*Found*Context*)
    val unexpectedSymbolExpr: string * string * string -> exn

    (*Expected*Found*lineNo*)
    val unexpectedSymbol: string * string * int-> exn

    val emptyQuery: unit -> exn
 
    (*Expected*Found*lineNo*) 
    val malformedQuery:string*string*int -> exn
  
    (*Expected*Found*lineNo*)
    val malformedMetadata:string*string -> exn

    (*Expected type*Found type*lineNo*file*)
    val typeErrorStoredData:string*string*string*int -> exn

    (*Filename*lineno*)
    val missingData:string*int -> exn

    val noSuchTable:unit-> exn
  
    (*Description*) 
    val multipleTablesNoMerge:unit -> exn 

    (*Description*)
    val invalidMerge:string -> exn

    (*Colname*Tablename*lineno*)
    val unknownColumn:string*string*int -> exn

    (*Description*)
    val invalidReferer:string -> exn

    (*Found type*Required type*)
    val pipeFunctionTypeError:string*string -> exn

    (*Name*)
    val pipeFunctionNotFound:string -> exn 

    (*Expected type*found type*field*file*)
    val typeErrorDefaultVal:string*string*string*string -> exn

    (*Filename*)
    val dataNotMatchingMetadata:string -> exn

    (*File*)
    val emptyDatafile:string -> exn

    (*Message*)
    val printMsgAndExit:string -> unit

 end;
