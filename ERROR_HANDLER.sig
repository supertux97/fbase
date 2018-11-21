fun main a = a;
signature ERROR_HANDLER = 
  sig
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

    (*Table name*)
    val noSuchTable:string -> exn
  
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
 end;
