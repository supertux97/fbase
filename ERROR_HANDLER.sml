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
    (*val aritmetric: string * int *)
 end;
