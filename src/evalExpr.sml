use "exprParser.sml";

fun main a = 
  let 
    val args = CommandLine.arguments()
  in 
    if length(args) <> 1 then
      print("Usage: ./evalExpr expression\n")
  else 
    print(evalToStr(List.nth(args,0)) ^ "\n")
  end

handle ErrorHandler.DivisionByZeroException(e) => ErrorHandler.printMsgAndExit(e)
      |ErrorHandler.NoSuchSymbolException(e) => ErrorHandler.printMsgAndExit(e)
      |ErrorHandler.UnexpectedSymbolException(e) => ErrorHandler.printMsgAndExit(e)
