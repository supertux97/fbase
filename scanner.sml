use "util/util.sml";
use "util/parseUtil.sml";
use "util/listUtil.sml";
use "utest.sml"; 
use "ErrorHandler.sml";
use "util/tokUtil.sml";

structure Scanner = 
struct
val keywords = ["from","filter","using","and","or",
              "merge","insert","rows","into","remove","as",
              "where", "set","create","table","with",
              "colums","of","default","string","boolean","number",
              "output"]
val functions = ["upper","lower","oneof","noneof"]
val pipeFunctions = ["upper","lower"]

val operatorsLenOne = ["+","-","*","/"]
val operators = operatorsLenOne
val stringSep = #"'"
val predicateOperatorsLenOne = ["=","<",">"]
val predicateOperatorsLenTwo = ["<=",">=", "!="]
val predicateOperators = predicateOperatorsLenOne @ predicateOperatorsLenTwo

val syntaxSymbolsLenOne = [".",",",":","{","}","(", ")","*","|",Char.toString(stringSep)] 
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
      val substr = Util.strToSs(x)
      val (noComment, comment) = Substring.splitl (fn c => c <> #"#") substr
    in
      Util.ssToStr(noComment) :: rmComments(xs)
    end
    |[] => []

 fun rmCommentsTailrec(lines: string list):string list =
   let fun rmTailRec(lines:string list, result:string list) = 
    case lines of
      (x::xs) =>
     let
        val substr = Util.strToSs(x)
        val (noComment, comment) = Substring.splitl (fn c => c <> #"#") substr
      in
        rmTailRec(xs, Util.ssToStr(noComment) :: result)
      end
      |[] => rev(result)
    in 
      rmTailRec(lines,[])
   end

fun getTok(tal: TokenAtLine) = 
 case tal of (t,_) => t 

fun getLineNo(tal:TokenAtLine) =
  case tal of(_,lineNo) => lineNo
fun convToken(tok:Token, f:(Token -> 'a)) = f(tok)

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
         |treeLit(TreeOper(oper)) => raise ErrorHandler.unexpectedSymbolExpr(
                                     "number",oper, source)
         |unknown => raise ErrorHandler.unexpectedSymbolExpr("number","other",source)
    end

fun mkTokAtLine(tok:Token, line:int):TokenAtLine = (tok, line)


(*Creates an expression tree for a list of tokens. 

 The function creates the tree by creating a parent node for the operator, a left child for the first value
 and a right child for the rest of the expression. Eg: 1+2+3 will result in the
 tree: Val:+ Left:1 Right:(Val:+ Left:2 Right:3)
 The function printTree can be used to get a textual representation of a tree
 in this style.

 High predenecence operations (division and multiplication) is evaluated before beeing inserted into the
 tree. Eg: 2*4+3 will result in the tree: Parent:+ Left:8 Right:3*)
fun createExprTree(toks: TokenAtLine list, expr:string):ExprTree = 
  let  fun createSingleNode(tok: TokenAtLine):ExprTree = 
             case getTok(tok) of
                   Litteral(Number(n)) => treeLit(TreeNum(n))
                  |Symbol(Operator(oper)) => treeLit(TreeOper(oper))
                  |other=> raise ErrorHandler.unexpectedSymbolExpr("number or operator",
                         TokUtil.tokToStr(other, TokUtil.tokValAndKind), expr)

       fun firstHiPredExprAndRest(toks:TokenAtLine list) =
              let val first = List.nth(toks,0)
                val second = List.nth(toks,1)
                val third = List.nth(toks,2)
                val rest = List.drop(toks,3)
            in
              (first,second,third,rest)
            end

       fun handleExpectedOperator(found:Token) = 
         raise
         ErrorHandler.unexpectedSymbolExpr("operrator",TokUtil.tokToStr(found,TokUtil.tokValAndKind),expr)

 in
   case toks of 
     [] => EmptyNode
    |[singleTok] => createSingleNode(singleTok)
    |(firstTok::afterFirstTok) => 
               let val secondTok = getTok(hd(afterFirstTok))
                   val rest = tl(afterFirstTok)
               in
                 case secondTok of
                   Symbol(Operator(oper)) => 
                        (*Expressions of high predecence is calculated beforehand*)
                        if ListUtil.member oper highPriOperators then 
                          let val (num1, opp, num2, rest) = firstHiPredExprAndRest(toks)
                              val hiPredEvaluated =
                                evalExprTree(Node(
                                    createExprTree([num1],expr),
                                    TreeOper(oper),
                                    createExprTree([num2],expr)),expr)
                           in
                             (*The result of the high predence expression is
                             added back to the list of tokens to parse*)
                            createExprTree(mkTokAtLine(Litteral(Number(hiPredEvaluated)),0)::rest,expr)
                           end
                        else 
                          Node(createExprTree([firstTok], expr), TreeOper(oper),
                          createExprTree(rest, expr))
                  |other => handleExpectedOperator(other)
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
          Util.format("Tree( Val:$ L:$ R:$ )", 
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
          if ListUtil.member firstTwoChars l2 then (firstTwoChars, String.substring(str, 2, size(str) -2)) 
          else if ListUtil.member firstChar l1 then (firstChar, String.substring(str,1, size(str) -1))
          else ("",str)
        end
      else if ListUtil.member (Char.toString(Util.hdString(str))) l1 
        then (Char.toString(Util.hdString str ), String.substring(str,1, size(str) -1))
      else getOperatorFromString(Util.rmHeadOfString str,l1,l2)

fun getTokenByKind(t:string):Token =
  let val tComparator = ListUtil.member(t)
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
      
    in 
      if ParseUtil.isStartOfIdentifier(firstChar) then
        let val (firstId, rest) = ParseUtil.getFirstIdentifier(str)
            val token = getTokenByKind(firstId)
        in (token, lineNo) :: scan(rest, lineNo)
        end

      else if ParseUtil.isStartOfwhitespace(firstChar) then scan(Util.rmHeadOfString(str), lineNo)

      else if ParseUtil.isStartOfString(firstChar) then
        let val ssNoFirstSep = Util.strToSs(Util.rmHeadOfString(str))
            val (strContent, rest) = Substring.splitl (fn c => c <> stringSep) ssNoFirstSep
        in  (Litteral(String(Util.ssToStr strContent)), lineNo) ::
        scan(Util.rmHeadOfString(Util.ssToStr(rest)),lineNo)
        end
      else if ParseUtil.isStartOfSubtraction(firstChar, secondCharOpt) then 
        let val (number,rest) = ParseUtil.getFirstNumberFromString(Util.rmHeadOfString(str))
        in  (getTokenByKind("+"),lineNo) ::
             (Litteral(Number(#1(ParseUtil.getFirstNumberFromString("-" ^
             Real.toString(number))))),lineNo):: scan(rest,lineNo)
        end
      else if ParseUtil.isStartOfDigit(firstChar) then
        let val (number, rest) = ParseUtil.getFirstNumberFromString(str)
        in ( Litteral(Number(number)), lineNo) :: scan(rest, lineNo)
        end

      else if ParseUtil.isStartOfnegativeDigit(firstChar,secondCharOpt ,thirdCharOpt) then 
        let val parenRemoved = Util.rmFirstCharMatchOfString(#")", Util.rmHeadOfString(str))
            val (number, rest) = ParseUtil.getFirstNumberFromString(parenRemoved)
        in ( Litteral(Number(number)), lineNo) :: scan(rest, lineNo)
        end

      else if ParseUtil.isNewline(firstChar) then scan(Util.rmHeadOfString(str), lineNo +1)

      else if ParseUtil.isStartofSymbol(firstChar,validSymbols) then 
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
    val noCommentOrWhitespace = rmWs(ListUtil.listToStr(noComments,Util.I," "))
  in
    scan(noCommentOrWhitespace,1)
  end

fun tokListToStr(tl:TokenAtLine list):string =
  case tl of
     [] => ""
    |(x::xs) =>
      case x of
        (tok, lineNo) =>
          Util.format("[$] $", [Util.$lineNo, TokUtil.tokToStr(tok,TokUtil.tokValAndKind)]) ^ "\n" ^ tokListToStr(xs)

fun evalFromTxt(txt:string):string =
  Real.toString(evalExprTree(createExprTree(trimAndScan(txt),txt),txt))

fun printTree(expr:string) = 
  print(exprTreeToStr(createExprTree(trimAndScan(expr),expr)))


fun testExpr(expr:string, ans:string) = test(expr, ans, evalFromTxt(expr),Util.I) 
fun main a = 
  let 
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

  in 
    a
  end

end;
