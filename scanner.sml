(*Description*)
use "util.sml";
use "utest.sml"; 
use "ErrorHandler.sml";

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

type TokenAtLine = Token * int;

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

val highPriOperators = ["*","/"]

(*Removes exess whitespace. Two or more whitespaces are squezed into one*)
fun rmWs(str:string):string =
  case (Util.getCharAtIndex(str,0), Util.getCharAtIndex(str,1))  of
      (SOME(#" "), SOME(#" ")) => rmWs(String.substring(str,1, size(str) -1))
    | (SOME(_), SOME(_)) => String.substring(str,0,1) ^ rmWs(String.substring(str,1,size(str) -1))
    | (_, _) => str (*The first or both are empty*)

fun rmWsTailRec(str:string):string = 
  let fun rmWsInner(str,strAcc) = 
    case (Util.getCharAtIndex(str,0), Util.getCharAtIndex(str,1))  of
      (SOME(#" "), SOME(#" ")) => rmWsInner(String.substring(str,1, size(str)-1) ,strAcc)
    | (SOME(_), SOME(_)) => rmWsInner(String.substring(str,1,size(str)-1), strAcc^ String.substring(str,0,1) )
    | (_, _) => strAcc(*The first or both are empty*)
  in
   rmWsInner(str,"") 
  end

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

fun getTok(tal: TokenAtLine) = 
 case tal of (t,_) => t 

fun getLineNo(tal:TokenAtLine) =
  case tal of(_,lineNo) => lineNo
fun convToken(tok:Token, f:(Token -> 'a)) = f(tok)

(*Gets the first tree tokens and then the rest*)
fun getFirstTripple(toks:TokenAtLine list):(TokenAtLine*TokenAtLine*TokenAtLine*TokenAtLine list) =
   ( List.nth(toks,0), List.nth(toks,1), List.nth(toks,2), Util.dropN(toks, 2))  

fun tokToStrWithType(t:Token) =
    let
       fun ts(value:string, kind:string) = Util.format("$:$", [value, kind])
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

  (*Node: Left Val Right*)
  datatype TreeLitteral = TreeNum of real | TreeOper of string
  datatype ExprTree = 
              EmptyNode |
              treeLit of TreeLitteral | 
              Node of ExprTree * TreeLitteral * ExprTree 

  fun evalExprTree(tree:ExprTree,source:string):real = 
    let fun solveBinExp(n1:real,oper:string,n2:real) = 
        case oper of 
          "+" => n1 + n2
         | "-" =>  n1 - n2
         | "*" => n1 * n2
         | "/" => if Real.==(0.0,n2) then 
                    raise ErrorHandler.divisionByZero(source)
                  else n1 / n2
         | unknown => raise ErrorHandler.noSuchSymbolExpr(unknown, source)
    in
      case tree of
          treeLit(TreeNum(n)) => n
         |Node(left,TreeOper(oper),right) => 
                      solveBinExp(evalExprTree(left,source), oper,
                      evalExprTree(right,source)) 
         |treeLit(TreeOper(oper)) => raise ErrorHandler.unexpectedSymbol(
                                     "number",oper, source)
         |unknown => raise ErrorHandler.unexpectedSymbol("number","other",source)
    end

fun mkTokAtLine(tok:Token, line:int):TokenAtLine = (tok, line)

fun createExprTree(toks: TokenAtLine list, expr:string):ExprTree = 
  let  fun createSingleNode(tok: TokenAtLine):ExprTree = 
             case getTok(tok) of
                   Litteral(Number(n)) => treeLit(TreeNum(n))
                  |Symbol(Operator(oper)) => treeLit(TreeOper(oper))
                  |unknown => raise ErrorHandler.unexpectedSymbol("number or operator",tokToStrWithType(unknown), expr)

       fun firstHiPredPiecePair(toks:TokenAtLine list) =
              let val first = List.nth(toks,0)
                val second = List.nth(toks,1)
                val third = List.nth(toks,2)
                val rest = List.drop(toks,3)
            in
              (first,second,third,rest)
            end

       fun handleExpectedOperator(found:Token) = 
         raise ErrorHandler.unexpectedSymbol("operrator",tokToStrWithType(found),expr)

 in
   case toks of 
     [] => EmptyNode
    |[x] => createSingleNode(x)
    |(x::xs) => 
              let val firstTok = getTok(x)
                  val secondTok = getTok(hd(xs))
               in
                 case secondTok of
                   Symbol(Operator(oper)) => 
                        (*Expressions of high predecence is calculated beforehand*)
                        if Util.member oper highPriOperators then 
                          let val (num1,opp,num2, rest) = firstHiPredPiecePair(toks)
                              val hiPredEvaluated =
                                evalExprTree(Node(createExprTree([num1],expr),TreeOper(oper),createExprTree([num2],expr)),expr)
                           in
                            createExprTree(mkTokAtLine(Litteral(Number(hiPredEvaluated)),0)::rest,expr)
                           end
                        else 
                          Node(createExprTree([x], expr), TreeOper(oper),
                          createExprTree(tl(xs), expr))
                  |unknown => handleExpectedOperator(unknown)
        end
    end

fun repeatStr(str:string, 0) = ""
  | repeatStr(str:string, 1) = str
  | repeatStr(str, n) = str ^ repeatStr(str,n-1) 


 fun exprTreeToStr(exprTree: ExprTree) = 
   let fun getValue(v:TreeLitteral) = 
         case v of 
           TreeNum(n)     => Real.toString(n)
          |TreeOper(oper) => oper 
   in
    case exprTree of
        Node(left,value,right) =>  
          Util.format("Tree( [$], L( $ ), R( $  )", 
            [getValue(value), exprTreeToStr(left),exprTreeToStr(right)])
       |EmptyNode => ""
       |treeLit(lit) => getValue(lit) 
   end

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
          if Util.member firstTwoChars l2 then (firstTwoChars, String.substring(str, 2, size(str) -2)) 
          else if Util.member firstChar l1 then (firstChar, String.substring(str,1, size(str) -1))
          else ("",str)
        end
      else if Util.member (Char.toString(Util.hdString(str))) l1 
        then (Char.toString(Util.hdString str ), String.substring(str,1, size(str) -1))
      else getOperatorFromString(Util.rmHeadOfString str,l1,l2)

fun getTokenByKind(t:string):Token =
  let val tComparator = Util.member(t)
  in
    if tComparator keywords then Keyword(t)
    else if tComparator functions then Function(t)
    else if tComparator pipeFunctions then PipeFunction(t)
    else if tComparator operators then Symbol(Operator(t))
    else if tComparator predicateOperators then Symbol(PredicateOperator(t))
    else if tComparator syntaxSymbols then Symbol(SyntaxSymbol(t))
    else Identifier(t)
  end

  fun strSubOption(str,idx) = 
    if idx < size(str) then SOME(String.sub(str,idx))
    else NONE

  fun scan(str:string, lineNo:int):TokenAtLine list =
    let
      val firstChar = String.sub(str, 0) 
      val secondCharOpt = strSubOption(str,1) 
      val thirdCharOpt = strSubOption(str, 2)  
      val subString = Substring.full(str)
      
      fun startOfIdentifier(c:char) = Char.isAlpha(firstChar) 
      fun whitespace(c:char) = c = #" "
      fun startOfString(c:char) = c = stringSep
      fun newline(c:char) = c = #"\n"
      fun startofSymbol(c:char) = Util.member (Char.toString(firstChar)) validSymbols
      fun startOfDigit(c:char) = Char.isDigit(firstChar)
      fun startOfSubtraction(c1:char,c2Opt:char option) = 
        case c2Opt of 
            SOME(c2) => c1 = #"-" andalso Char.isDigit(c2)
           |NONE => false 
      fun startOfnegativeDigit(first:char, secondOpt:char option, thirdOpt:char option ) = 
        case (secondOpt, thirdOpt) of
            (SOME(c2), SOME(c3)) =>  first= #"(" andalso c2 = #"-" andalso Char.isDigit(c3)
           | (_, _) => false
    in 
      if startOfIdentifier(firstChar) then
        let val (alfaToken, rest) = Substring.splitl Char.isAlpha subString
            val token = getTokenByKind(Util.ssToStr(alfaToken))
        in (token, lineNo) :: scan(Util.ssToStr(rest), lineNo)
        end

      else if whitespace(firstChar) then scan(Util.rmHeadOfString(str), lineNo)

      else if startOfString(firstChar) then
        let val ssNoFirstSep = Util.strToSs(Util.rmHeadOfString(str))
            val (strContent, rest) = Substring.splitl (fn c => c <> stringSep) ssNoFirstSep
        in  (Litteral(String(Util.ssToStr strContent)), lineNo) ::
        scan(Util.rmHeadOfString(Util.ssToStr(rest)),lineNo)
        end
      else if startOfSubtraction(firstChar, secondCharOpt) then 
        let val (number,rest) = Util.getFirstNumberFromString(Util.rmHeadOfString(str))
        in  (getTokenByKind("+"),lineNo) ::
             (Litteral(Number(#1(Util.getFirstNumberFromString("-" ^
             Real.toString(number))))),lineNo):: scan(rest,lineNo)
        end
      else if startOfDigit(firstChar) then
        let val (number, rest) = Util.getFirstNumberFromString(str)
        in ( Litteral(Number(number)), lineNo) :: scan(rest, lineNo)
        end

      else if startOfnegativeDigit(firstChar,secondCharOpt ,thirdCharOpt) then 
        let val parenRemoved = Util.rmFirstCharMatchOfString(#")", Util.rmHeadOfString(str))
            val (number, rest) = Util.getFirstNumberFromString(parenRemoved)
        in ( Litteral(Number(number)), lineNo) :: scan(rest, lineNo)
        end

      else if newline(firstChar) then scan(Util.rmHeadOfString(str), lineNo +1)

      else if startofSymbol(firstChar) then 
          let val (symbol, rest) = getOperatorFromString(str, oneLenOperators, twoLenOperators)
          in (getTokenByKind(symbol),lineNo) :: scan(rest, lineNo)
          end

      else 
        raise ErrorHandler.noSuchSymbol(Char.toString(firstChar),lineNo)
    end
    handle Subscript => []

exception UnexpectedTokensException of string * int * int
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
    val noComments = rmComments(Util.splitStrByNewline(str))
    val noCommentOrWhitespace = rmWs(Util.listToStr(noComments,Util.I," "))
  in
    scan(noCommentOrWhitespace,1)
  end

fun tokListToStr(tl:TokenAtLine list):string =
  case tl of
     [] => ""
    |(x::xs) =>
      case x of
        (tok, lineNo) =>
          Util.format("[$] $", [Util.$lineNo, tokToStrWithType(tok)]) ^ "\n" ^ tokListToStr(xs)

fun evalFromTxt(txt:string):string =
  Real.toString(evalExprTree(createExprTree(trimAndScan(txt),txt),txt))

fun printTree(expr:string) = 
  print(exprTreeToStr(createExprTree(trimAndScan(expr),expr)))

fun testExpr(expr:string, ans:string) = test(expr, ans, evalFromTxt(expr),Util.I) 

val _ = testExpr("2*6","12.0")
val _ = testExpr("1.22+2.44","3.66")
val _ = testExpr("1+2+3","6.0")
val _ = testExpr("1.123+4.16+8.1+9","22.383")
val _ = testExpr("1+2*3","7.0")
val _ = testExpr("32/8/5","0.8")
val _ = testExpr("1-2-3-4","~8.0")
val _ = testExpr("1+2*3", "7.0")
val _ = testExpr("(-2)/4/2","~0.25")
val _ = testExpr("(-2)*(-6)","12.0")
val _ = testExpr("(-2)*(-6)*(-12.45)","~149.4")
val _ = testExpr("12/6/4","0.5")
val _ = testExpr("1-2-3-4-5-6-7-8-9","~43.0")
