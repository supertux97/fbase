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
    Identifier of string(*Ex: tablename, columname*)
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

exception NoSuchSymolError of string * int
exception InvalidSyntaxError of string * int
exception ConvesionFormatException of string

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
(*
datatype Expr = Num of real|
                Mul of real * real|
                Div of real * real|
                Add of real * real|
                Sub of real * real; *)
(*Tries to remove the first n elements from a list*) 
fun dropN([], n:int) = []
  | dropN(l,0) = l
  | dropN(x::xs,n) = dropN(xs,n-1) 
 
fun getTok(tal: TokenAtLine) = 
 case tal of (t,_) => t 

fun getLineNo(tal:TokenAtLine) =
  case tal of(_,lineNo) => lineNo
fun convToken(tok:Token, f:(Token -> 'a)) = f(tok)

(*Gets the first tree tokens and then the rest*)
fun getFirstTripple(toks:TokenAtLine list):(TokenAtLine*TokenAtLine*TokenAtLine*TokenAtLine list) =
   ( List.nth(toks,0), List.nth(toks,1), List.nth(toks,2), dropN(toks, 2))  

exception UnexpectedSymbol of string * int;

fun handleUnexpected(found:string,wanted:string,lineNo:int) = raise
  UnexpectedSymbol(found, lineNo)

  (*
fun splitExpr(toks:TokenAtLine list):Expr list = 
  let 
    val hightPredOper = ["*","/"]
    val lowPredOper = ["-","+","**"]
    
    fun getExprByOp(n1,n2,oper,lineNo) =
        case oper of
             "*" => Mul(n1,n2)
          | "/" => Div(n1,n2)
          | "-" => Sub(n1,n2)
          | "+" => Add(n1,n2)
          | s => raise UnexpectedSymbol("The",lineNo) 

    fun splitByHiOperPred(toks:TokenAtLine list):Expr list = 
      let 
        val (talNum1,talOper,talNum2,rest) = getFirstTripple(toks)
        val talToNum = fn(tal) =>
           case getTok(tal) of 
                Litteral(Number(n)) => n 
              | _ => handleUnexpected("erer","jejr",getLineNo(tal)) 
        val talToOper= fn(tal) => 
            case getTok(tal) of 
                 Symbol(Operator(opp)) => opp 
               | _ => handleUnexpected("erier","34j3j4",getLineNo(tal)) 
        val(n1,oper,n2) = (talToNum(talNum1), talToOper(talOper),talToNum(talNum2)) 
      in
        if length(toks) = 0 then [] 
        else 
          case getTok(talOper) of 
            Symbol(Operator(opVal)) => if member opVal hightPredOper 
                          then getExprByOp(n1,n2,oper,getLineNo(talOper)) :: splitByHiOperPred(rest)
                          else splitByHiOperPred(rest)
            |_ => raise UnexpectedSymbol("4u4u4",getLineNo(talOper))
      end
  in
    splitByHiOperPred(toks)
  end  *)

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
  datatype Expr = Num of real | MathExpr of Expr * symbol * Expr 


 (* fun solveExpr(expr:Expr):real = 
    let 
      fun solveBinExp(n1:real, n2:real, op:string) = 
        case op of 
             "+" => n1 + n2
           | "-" => n1 - n2
           | "*" => n1 * n2
           | "/" => n1 / n2

      fun solveSingle(exp:Expr):real = 
        case exp of 
              Num(n) => n
             |MathExpr(me) => 
              case me of
                (Num(n1),Operator(op), Num(n2)) => solveBinExp(n1,n2,op)
                |(Expr(e1),Operator(op),Expr(e2)) => 
                    solveSingle(Expr(solveSingle(e1),op, solveSingle(e2)))
      in
         solveSingle(expr)  
    end *)

exception UnexpectedTokensException of string * int * int

fun tokListToExpr( tokList: TokenAtLine list):Expr = 
  case tokList of
       [x] => 
        (case getTok(x) of 
             Litteral(Number(n)) => Num(n)
           | t => raise InvalidSyntaxError(format("Expected number but got $",
               [tokToStrWithType(t)]),getLineNo(x)))
        |(x::xs) => 
          let val tok1 = getTok(x)
              val tok2 = getTok(hd(xs))
              val rest = tl(xs)
          in
            case (tok1,tok2) of
                 ( Litteral(Number(n)), Symbol(Operator(oper)) ) => 
                    MathExpr(Num(n),Operator(oper),tokListToExpr(tl(rest)))
                | (tok1,tok2) => raise
                    UnexpectedTokensException(format("Expected number operator got $ $",[tokToStrWithType(tok1),tokToStrWithType(tok2)] ),
                       getLineNo(x), getLineNo(hd(xs)))
          end
       | [] => Num(0.0)

fun exprToStr(exp:Expr) = 
  case exp of 
       Num(n) => Real.toString(n)
     | mathexpr as MathExpr(e1, Operator(ops),e2) =>
           "( Expr(" ^ exprToStr(e1) ^ " " ^ ops^ exprToStr(e2) ^ ")"
     | _ => " "

  val tokList:Token list = [Litteral(Number(1.0)),
                            Symbol(Operator("*")),
                             Litteral(Number(5.0)),
                             Symbol(Operator("+")),
                             Litteral(Number(9.0))]

 val exp = MathExpr(Num(1.0),Operator("*"), MathExpr(Num(5.0), Operator("+"),Num(9.0)))
 
val _ = print(exprToStr(exp))

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


(*A version which uses the Real-libarys function for parsing, but resturns the
real or raises an exeption instead of returning a option*)
fun realFromString(str:string):real =
 case Real.fromString(str) of
     SOME(r) => r
   | NONE => raise ConvesionFormatException(format("Could not parse the string intoa real", [str]))

 fun ssToStr(s:substring) = Substring.string s
 fun strToSs(s:string) = Substring.full s

 (*Parses the first number (including decimal  and negative numbers) from a string.
 The eventually non-numeric  characters(excluding -) at the start is discarded
 The number and the rest of the string is returned*)
fun getFirstNumberFromString(str:string):(real*string) =
  let
   fun digitOrMinus c = Char.isDigit(c) orelse c = #"-"
   fun nextNumPair(str:string):(string*string) =
     let val (num, rest) = Substring.splitl digitOrMinus (strToSs str)
     in (ssToStr num, ssToStr rest)
     end
 in
   if digitOrMinus(hdString(str)) then
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
      val secondChar = String.sub(str, 1)
      val subString = Substring.full(str)

      fun startOfIdentifier(c:char) = Char.isAlpha(firstChar) 
      fun whitespace(c:char) = c = #" "
      fun startOfString(c:char) = c = stringSep
      fun newline(c:char) = c = #"\n"
      fun startofSymbol(c:char) = member (Char.toString(firstChar)) validSymbols
      fun startOfDigit(c:char) = Char.isDigit(firstChar)
      fun startOfnegativeDigit(sign:char, startDigit:char) = sign = #"-" andalso
        Char.isDigit(startDigit)
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

      else if startOfDigit(firstChar) orelse startOfnegativeDigit(firstChar, secondChar) then
        let val (number, rest) = getFirstNumberFromString(str)
        in ( Litteral(Number(number)), lineNo) :: scan(rest, lineNo)
        end

      else if newline(firstChar) then scan(rmHeadOfString(str), lineNo +1)

      else if startofSymbol(firstChar) then 
          let val (symbol, rest) = getOperatorFromString(str, oneLenOperators, twoLenOperators)
          in (getTokenByKind(symbol),lineNo) :: scan(rest, lineNo)
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
