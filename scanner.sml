(*Description*)
use "util.sml";
fun main a = print("Hello sml");

datatype litteralType =
  String of string
  | Bool of bool
  | Number of real

datatype symbol =
    Operator of string
  | PredicateOperator of string
  | SyntaxSymbol of string

datatype Token =
    Identifier of string (*Ex: tablename, columname*)
  | Function of string (*Ex: noneof*)
  | PipeFunction of string (*Ex: upper, lower*)
  | Litteral of litteralType (*Ex: 123, "text"*)
  | Keyword of string (*Ex: from,Token filter*)
  | Symbol of symbol (*Ex: #, {*)

type TokenAtLine = Token * int

val keywords = ["from","filter","using","and","or",
              "merge","insert","rows","into","remove","as",
              "where", "set","create","table","with",
              "colums","of","default","string","boolean","number",
              "output"]
val functions = ["upper","lower","oneof","noneof"]
val stringSep = #"'"
val pipeFunctions = ["upper","lower"]

val operatorsLenOne = ["+","-","*","/"]
val operators = operatorsLenOne

val predicateOperatorsLenOne = ["=","<",">"]
val predicateOperatorsLenTwo = ["<=",">=", "!="]
val predicateOperators = predicateOperatorsLenOne @ predicateOperatorsLenTwo

val syntaxSymbolsLenOne = [".",",",":","{","}","(", ")","*","|", Char.toString stringSep] 
val syntaxSymbolsLenTwo = ["->"]
val syntaxSymbols = syntaxSymbolsLenOne @ syntaxSymbolsLenTwo 

val oneLenOperators = operatorsLenOne @ predicateOperatorsLenOne @ syntaxSymbolsLenOne
val twoLenOperators = predicateOperatorsLenTwo @ syntaxSymbolsLenTwo 
val firstOfTwoLenOperators = map (fn e => Char.toString(String.sub(e,0))) twoLenOperators
val validSymbols = predicateOperators @ syntaxSymbols @ operators @ firstOfTwoLenOperators
fun rmWs(str:string):string =
  case (getCharAtIndex(str,0), getCharAtIndex(str,1))  of
      (SOME(#" "), SOME(#" ")) => rmWs(String.substring(str,1, size(str) -1))
    | (SOME(_), SOME(_)) => String.substring(str,0,1) ^ rmWs(String.substring(str,1,size(str) -1))
    | (_, _) => str (*The first or both are empty*)

 fun rmComments(lines: string list):string list =
  case lines of
    (x::xs) =>
    let
      val substr = Substring.full(x)
      val (noComment, comment) = Substring.splitl (fn c => c <> #"#") substr
    in
      Substring.string noComment :: rmComments(xs)
    end
    |(nil) => lines

fun splitStrByNewline(str:string):string list =
  let val substr = Substring.full(str)
  in
    map (fn e => Substring.string e) (Substring.fields (fn c => c = #"\n") substr )
  end

fun getTokenByKind(t:string):Token =
  let val tComparator = member(t)
  in
    if tComparator keywords then Keyword(t)
    else if tComparator functions then Function(t)
    else if tComparator pipeFunctions then PipeFunction(t)
    else if tComparator operators then Symbol(Operator(t))
    else if tComparator predicateOperators then Symbol(PredicateOperator(t))
    else if tComparator syntaxSymbols then Symbol(SyntaxSymbol(t))
    else Identifier(t)
  end

fun tokToStrWithType(t:Token) =
    let
       fun ts(value:string, kind:string) = format("$:$", [value, kind])
    in
     case t of
      Identifier i => ts(i, "Id")
    | Function f => ts(f, "Fun")
    | PipeFunction pf => ts(pf, "PipeFun")
    | Keyword k => ts(k,"Keyword")
    | Litteral l =>
      ( case l of
         String s => ts(s, "String")
        |Bool b => ts( if b then "true" else "false", "Bool")
        |Number n => ts(Real.toString(n), "Num") )
    |Symbol s =>
          (case s of
             Operator opp => ts(opp, "Op")
            | PredicateOperator po => ts(po, "PredOp")
            | SyntaxSymbol ss => ts(ss, "Syntax"))
      end

exception NoSuchSymolError of string * int
exception ConvesionFormatException of string
(*A version which uses the Real-libarys function for parsing, but resturns the
real or raises an exeption instead of returning a option*)
fun realFromString(str:string):real =
 case Real.fromString(str) of
     SOME(r) => r
   | NONE => raise ConvesionFormatException(format("Could not parse the string intoa real", [str]))

 fun ssToStr(s:substring) = Substring.string s
 fun strToSs(s:string) = Substring.full s

 (*Parses the first number (including decimal number) from a string.
 The eventually non-numeric characters at the start is discarded
 The number and the rest of the string is returned*)
fun getFirstNumberFromString(str:string):(real*string) =
  let
   fun nextNumPair(str:string):(string*string) =
     let val (num, rest) = Substring.splitl Char.isDigit (strToSs str)
     in (ssToStr num, ssToStr rest)
     end
 in
   if Char.isDigit( hdString str ) then
     let val (integerPart, rest) = nextNumPair str
     in
       if size(rest) > 0 andalso hdString rest = #"." then
         let val (decimalPart, restAfterDecimal) = nextNumPair( rmHeadOfString rest)
         in (realFromString(format("$.$", [integerPart, decimalPart])), restAfterDecimal)
         end
       else (realFromString(format("$.0",[integerPart])), rest)
     end
   else getFirstNumberFromString(rmHeadOfString(str))  (*Drop first char and retry*)
 end

  (*Returns a pair of the first string surrounded by sep, and the rest.
   The separator itself is removed fro mboth parts and characters at the start
   of the string not matching sep is also removed.
   If the separator is not found, two empty strings are returned
  fun splitBySep(str:string, sep:string) =
    if hdString str = sep then
      let
        val ssNoFirstSep = strToSs (rmHeadOfString(str))
        val (strNoSeps, restnoSep) = Substring.splitl ssNoFirstSep fn c => c != sep
      in
        (strNoSeps,)
    else if size(str) == 0 then ("","")
    else splitBySep(rmHeadOfString str, sep) *)

  (*Get the first operator from a string and the rest of the string
   Unwanted chars are removed unitil a valid symbol occurs.

   The function will first read one symbol and then the next (if any)
   A two-part sybol is retuned if the two chars is part of two-symbol list, the
   one-part if just the first is part of one-part list or an emptry string if
   none of the above is true*)
  fun getOperatorFromString(str:string,l1:string list, l2:string list):(string*string) = 
      if size(str) = 0 then ("","")
      else if size(str) >= 2 then
        let val firstTwoChars = String.substring(str,0,2)
            val firstChar = String.substring(str,0,1)
        in 
          if member firstTwoChars l2 then (firstTwoChars, String.substring(str, 2, size(str) -2)) 
          else if member firstChar l1 then (firstChar, String.substring(str,1, size(str) -1))
          else ("",str)
        end
      else if member (Char.toString(hdString(str))) l1 
        then (Char.toString(hdString str ), String.substring(str,1, size(str) -1))
      else getOperatorFromString(rmHeadOfString str,l1,l2)

  fun scan(str:string, lineNo:int):TokenAtLine list =
    let
      val firstChar = String.sub(str, 0)
      val subString = Substring.full(str)

      fun startOfIdentifier(c:char) = Char.isAlpha(firstChar) 
      fun whitespace(c:char) = c = #" "
      fun startOfString(c:char) = c = stringSep
      fun newline(c:char) = c = #"\n"
      fun startofSymbol(c:char) = member (Char.toString(firstChar)) validSymbols
      fun startOfDigit(c:char) = Char.isDigit(firstChar) 
    in 
      if startOfIdentifier(firstChar) then
        let val (alfaToken, rest) = Substring.splitl Char.isAlpha subString
            val token = getTokenByKind(ssToStr(alfaToken))
        in (token, lineNo) :: scan(ssToStr(rest), lineNo)
        end

      else if whitespace(firstChar) then scan(rmHeadOfString(str), lineNo)

      else if startOfString(firstChar) then
        let val ssNoFirstSep = strToSs(rmHeadOfString(str))
            val (strContent, rest) = Substring.splitl (fn c => c <> stringSep) ssNoFirstSep
        in  (Litteral(String(ssToStr strContent)), lineNo) :: scan(rmHeadOfString(ssToStr(rest)),lineNo)
        end

      else if newline(firstChar) then scan(rmHeadOfString(str), lineNo +1)

      else if startofSymbol(firstChar) then 
          let val (symbol, rest) = getOperatorFromString(str, oneLenOperators, twoLenOperators)
          in (getTokenByKind(symbol),lineNo) :: scan(rest, lineNo)
          end

      else if startOfDigit(firstChar) then
        let val (number, rest) = getFirstNumberFromString(str)
        in ( Litteral(Number(number)), lineNo) :: scan(rest, lineNo)
        end

      else 
        raise NoSuchSymolError(format("Unknown symbol $", [Char.toString(firstChar)]), lineNo)
    end
    handle Subscript => []

fun cat s =
  let
    val f = TextIO.openIn s
    and c = ref ""
  in
    while (c := TextIO.inputN (f, 1); !c <> "") do
      TextIO.output (TextIO.stdOut, !c);
    TextIO.closeIn f
  end;

fun trimAndScan(str:string):TokenAtLine list =
  let
    val noComments = rmComments(splitStrByNewline(str))
    val noCommentOrWhitespace = rmWs(listToStr(noComments,I," "))
  in
    scan(str,1)
  end

val q1 = "from Person as P\nfilter salary > 100\noutput P.adress, P.firstname"
val q:string  = fileToStr("q1.txt")

fun strTokenList(tl:TokenAtLine list):string =
  case tl of
     [] => ""
    |(x::xs) =>
      case x of
        (tok, lineNo) =>
          format("[$] $", [$lineNo, tokToStrWithType(tok)]) ^ "\n" ^ strTokenList(xs)

val _ = print(strTokenList(trimAndScan(q)));
