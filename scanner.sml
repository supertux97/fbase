(*Description*)
use "util.sml";
use "utest.sml"; 
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

val highPriOperators = ["*","/"]
exception NoSuchSymolError of string * int
exception InvalidSyntaxError of string * int

(*Removes exess whitespace. Two or more whitespaces are squezed into one*)
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

fun getTok(tal: TokenAtLine) = 
 case tal of (t,_) => t 

fun getLineNo(tal:TokenAtLine) =
  case tal of(_,lineNo) => lineNo
fun convToken(tok:Token, f:(Token -> 'a)) = f(tok)

(*Gets the first tree tokens and then the rest*)
fun getFirstTripple(toks:TokenAtLine list):(TokenAtLine*TokenAtLine*TokenAtLine*TokenAtLine list) =
   ( List.nth(toks,0), List.nth(toks,1), List.nth(toks,2), dropN(toks, 2))  

exception UnexpectedSymbol of string * int;
exception MalformedExpression of string;

fun handleUnexpected(found:string,wanted:string,lineNo:int) = raise
  UnexpectedSymbol(found, lineNo)

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
fun exprToStr(exp:Expr) = 
  case exp of 
       Num(n) => Real.toString(n)
     | mathexpr as MathExpr(e1, Operator(ops),e2) =>
           "( Expr(" ^ exprToStr(e1) ^ " " ^ ops ^ " " ^ exprToStr(e2) ^ ")"
     | _ => " "

  exception unknownSymbolException of string 
  exception wrongFormatException of string

fun evalExpr(expr:Expr):real = 
  let fun solveBinExp(n1:real,oper:string,n2:real) = 
        case oper of 
          "+" => n1 + n2
         | "-" => n1 - n2
         | "*" => n1 * n2
         | "/" => n1 / n2
         | unknown => raise unknownSymbolException(format("Unnown symbol: '$' found in expression: $",
          [unknown, exprToStr(expr)]))
  in
    case expr of 
      Num(n) => n
     |MathExpr(me) => 
        case me of
           (Num(n1), Operator(oper), Num(n2)) => solveBinExp(n1,oper,n2)
          |(Num(n1), Operator(oper),me as MathExpr(_,_,_)) =>
              solveBinExp(n1,oper,evalExpr(me))
          | _ => raise wrongFormatException("Wrong format in expression: " ^ exprToStr(expr))
  end

  exception UnexpectedTokenException of string
  (*Node: Left Val Right*)
  datatype TreeLitteral = TreeNum of real | TreeOper of string
  datatype ExprTree = 
              EmptyNode |
              treeLit of TreeLitteral | 
              Node of ExprTree * TreeLitteral * ExprTree 

  fun createExprTree(toks: TokenAtLine list):ExprTree = 
    let  fun firstHiPredPiecePair(toks:TokenAtLine list):(TokenAtLine list*TokenAtLine list) = 
          let val first = List.nth(toks,0)
              val second = List.nth(toks,1)
              val third = List.nth(toks,2)
              val rest = List.drop(toks,3)
          in
            ([first,second,third], rest)
          end
         fun createExprTreeFromRevList(revToks:TokenAtLine list):ExprTree = 
           case revToks of 
             [] => EmptyNode
             | [x] => (
                 case getTok(x) of
                    Litteral(Number(n)) => treeLit(TreeNum(n))
                  |Symbol(Operator(oper)) => treeLit(TreeOper(oper))
                  |unknown => raise UnexpectedTokenException(formatln("Expected number but got $", [tokToStrWithType(unknown)])) 
                )
            | (x::xs) => 
               let val firstTok = getTok(x)
                   val middleTok = getTok(hd(xs))
              in
                 case middleTok of
                      Symbol(Operator(oper)) => 
                        if member oper highPriOperators then 
                          let val (hiPrecToks, rest) = firstHiPredPiecePair(revToks)
                          in
                            if null rest then 
                              case hiPrecToks of 
                                   (x::xs) => 
                                     Node(createExprTree([x]),( 
                                            case getTok(hd(xs)) of  
                                               Symbol(Operator(oper)) =>
                                                 TreeOper(oper)
                                               |t => raise
                                                    UnexpectedTokenException("Expected operator but got " ^
                                                 tokToStrWithType(t))),
                                               createExprTree(tl(xs)))
                                          |[] => raise MalformedExpression("")
                            else 
                              let val restFirstOp = case getTok(hd(rest)) of
                                                     Symbol(Operator(oper)) => oper
                                                   | tok => raise
                                                   UnexpectedTokenException("Expected operator but found " ^
                                                   tokToStrWithType(tok))
                              in 
                              Node(createExprTree(hiPrecToks),
                                TreeOper(restFirstOp),
                                createExprTreeFromRevList(tl(rest)))
                              end
                          end
                        else 
                          Node(createExprTreeFromRevList([x]), TreeOper(oper),
                          createExprTreeFromRevList(tl(xs)))
                    | unknown => 
                      raise UnexpectedTokenException(formatln("Expected symbol but got $",[tokToStrWithType(middleTok)]))
              end
       in
      createExprTreeFromRevList(toks)
    end

  fun evalExprTree(tree:ExprTree,source:string):real = 
    let fun solveBinExp(n1:real,oper:string,n2:real) = 
        case oper of 
          "+" => n1 + n2
         | "-" => n1 - n2
         | "*" => n1 * n2
         | "/" => n1 / n2
         | unknown => raise unknownSymbolException(format("Unnown symbol: '$' found in expression: $",
          [unknown, source]))
    in
      case tree of
          treeLit(TreeNum(n)) => n
         |Node(left,TreeOper(oper),right) => 
                      solveBinExp(evalExprTree(left,source), oper,
                      evalExprTree(right,source)) 
         |treeLit(TreeOper(oper)) => 
             raise MalformedExpression(formatln("Found  $ but expected number in expression: $",
            [oper,source] ))
         |_ => raise MalformedExpression(formatln("in expression $", [source]))
    end

fun repeatStr(str:string, 0) = ""
  | repeatStr(str:string, 1) = str
  | repeatStr(str, n) = str ^ repeatStr(str,n-1) 


 fun exprTreeToStr(exprTree: ExprTree) = 
   let fun treeLitToStr(lit:TreeLitteral) = 
      case lit of 
         TreeNum(n) => Real.toString(n)
        |TreeOper(oper) => oper 
      fun pad(num) = repeatStr(" ",num)
   in
    case exprTree of
        Node(left,lit,right) =>  
          format("Tree( [$], L( $ ), R( $  )", 
            [treeLitToStr(lit), exprTreeToStr(left), exprTreeToStr(right)])
       |EmptyNode => ""
       |treeLit(lit) => treeLitToStr(lit) 
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
          if member firstTwoChars l2 then (firstTwoChars, String.substring(str, 2, size(str) -2)) 
          else if member firstChar l1 then (firstChar, String.substring(str,1, size(str) -1))
          else ("",str)
        end
      else if member (Char.toString(hdString(str))) l1 
        then (Char.toString(hdString str ), String.substring(str,1, size(str) -1))
      else getOperatorFromString(rmHeadOfString str,l1,l2)

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
      fun startofSymbol(c:char) = member (Char.toString(firstChar)) validSymbols
      fun startOfDigit(c:char) = Char.isDigit(firstChar)
      fun startOfnegativeDigit(first:char, secondOpt:char option, thirdOpt:char option ) = 
        case (secondOpt, thirdOpt) of
            (SOME(c2), SOME(c3)) =>  first= #"(" andalso c2 = #"-" andalso Char.isDigit(c3)
           | (_, _) => false
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
      
      else if startOfDigit(firstChar) then
        let val (number, rest) = getFirstNumberFromString(str)
        in ( Litteral(Number(number)), lineNo) :: scan(rest, lineNo)
        end

      else if startOfnegativeDigit(firstChar,secondCharOpt ,thirdCharOpt) then 
        let val parenRemoved = rmFirstCharMatchOfString(#")", rmHeadOfString(str))
            val (number, rest) = getFirstNumberFromString(parenRemoved)
        in ( Litteral(Number(number)), lineNo) :: scan(rest, lineNo)
        end

      else if newline(firstChar) then scan(rmHeadOfString(str), lineNo +1)

      else if startofSymbol(firstChar) then 
          let val (symbol, rest) = getOperatorFromString(str, oneLenOperators, twoLenOperators)
          in (getTokenByKind(symbol),lineNo) :: scan(rest, lineNo)
          end

      else 
        raise NoSuchSymolError(formatln("Unknown symbol $", [Char.toString(firstChar)]), lineNo)
    end
    handle Subscript => []

exception UnexpectedTokensException of string * int * int

fun tokListToExpr( tokList: TokenAtLine list):Expr = 
  case tokList of
       [x] => 
        (case getTok(x) of 
             Litteral(Number(n)) => Num(n)
           | t => raise InvalidSyntaxError(formatln("Expected number but got $",
               [tokToStrWithType(t)]),getLineNo(x)))
        |(x::xs) => 
          let val tok1 = getTok(x)
              val tok2 = getTok(hd(xs))
              val rest = tl(xs)
          in
            case (tok1,tok2) of
                 ( Litteral(Number(n)), Symbol(Operator(oper)) ) => 
                    MathExpr(Num(n),Operator(oper),tokListToExpr(rest))
                | (tok1,tok2) => raise
                    UnexpectedTokensException(formatln("Expected number operator got $ $",[tokToStrWithType(tok1),tokToStrWithType(tok2)] ),
                       getLineNo(x), getLineNo(hd(xs)))
          end
       | [] => Num(0.0)


  val tokList:Token list = [Litteral(Number(1.0)),
                            Symbol(Operator("*")),
                             Litteral(Number(5.0)),
                             Symbol(Operator("+")),
                             Litteral(Number(9.0))]

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

fun tokListToStr(tl:TokenAtLine list):string =
  case tl of
     [] => ""
    |(x::xs) =>
      case x of
        (tok, lineNo) =>
          format("[$] $", [$lineNo, tokToStrWithType(tok)]) ^ "\n" ^ tokListToStr(xs)

fun evalFromTxt(txt:string):string =
  Real.toString(evalExprTree(createExprTree(trimAndScan(txt)),txt))

fun printTree(expr:string) = 
  print(exprTreeToStr(createExprTree(trimAndScan(expr))))

fun testExpr(expr:string, ans:string) = test(expr, ans, evalFromTxt(expr),I) 

val _ = testExpr("1+1","2.0")
val _ = testExpr("2*6","12.0")
val _ = testExpr("1.22+2.44","3.66")
val _ = testExpr("1+2+3","6.0")
val _ = testExpr("1.123+4.16+8.1+9","22.383")
val _ = testExpr("1+2*3","7.0")
val _ = testExpr("32/8/5","0.8")
   
val _ = printTree("32/8/5")

