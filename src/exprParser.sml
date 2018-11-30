use "scanner.sml";
val highPriOperators = ["*","/"]
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
             case TokUtil.getTok(tok) of
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
               let val secondTok = TokUtil.getTok(hd(afterFirstTok))
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
                            createExprTree(TokUtil.mkTokAtLine(Litteral(Number(hiPredEvaluated)),0)::rest,expr)
                           end
                        else 
                          Node(createExprTree([firstTok], expr), TreeOper(oper),
                          createExprTree(rest, expr))
                  |other => handleExpectedOperator(other)
        end
    end

(*Creates a string repsetntation of a expression tree. Be aware that some parts
  of the expression may have been evaluated beforehand. This is the case for
  the high predecence operators division and multiplication*)
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

fun evalToStr(expr:string):string =
  Real.toString(evalExprTree(createExprTree(Scanner.trimAndScan(expr),expr),expr))

fun printTree(expr:string) = 
  print(exprTreeToStr(createExprTree(Scanner.trimAndScan(expr),expr)))

fun testExpr(expr:string, ans:string) = test(expr, ans, evalToStr(expr),Util.I) 

fun doUnitTests() = 
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
    1
  end

fun main a = 
  let 
    val args = CommandLine.arguments()
  in 
    if length(args) <> 1 then
      print("Usage: exprParser.sml expression\n")
  else 
    print(evalToStr(List.nth(args,0)) ^ "\n")
  end

