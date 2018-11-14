use "exprEvaluator.sml";
(*Debugging*)
fun printTree(expr:string) = 
  print(exprTreeToStr(createExprTree(Scanner.trimAndScan(expr),expr)))

fun testExpr(expr:string, ans:string) = test(expr, ans, evalFromString(expr),Util.I) 
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
