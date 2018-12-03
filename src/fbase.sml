use "query.sml";

fun main a = 
  let 
    val args = CommandLine.arguments()
  in 
    if length(args) <> 1 then
      print("Usage: query.sml queryfile\n")
  else 
    Query.runQueryFromFile(List.nth(args,0))
  end

(*Workaround to prevent the exception from beeing always thrown two times*)
(*I am fully aware that this is a little maintaiable solution, in regards to
adding new exceptions in the future. But at least the exceptions are still
thrown when not listet here, just two times.*)
handle ErrorHandler.UnknownColumnException(e)=> ErrorHandler.printMsgAndExit(e)
  |ErrorHandler.DivisionByZeroException(e)=> ErrorHandler.printMsgAndExit(e)
  |ErrorHandler.UnexpectedSymbolException(e)=> ErrorHandler.printMsgAndExit(e)
  |ErrorHandler.NoSuchSymbolException(e)=> ErrorHandler.printMsgAndExit(e)
  |ErrorHandler.AritmetricException(e)=> ErrorHandler.printMsgAndExit(e)
  |ErrorHandler.EmptyQueryException(e)=> ErrorHandler.printMsgAndExit(e)
  |ErrorHandler.MalformedQueryException(e)=> ErrorHandler.printMsgAndExit(e)
  |ErrorHandler.MalformedMetadataException(e)=> ErrorHandler.printMsgAndExit(e)
  |ErrorHandler.TypeErrorStoredDataException(e)=> ErrorHandler.printMsgAndExit(e)
  |ErrorHandler.NoSuchTableException(e)=> ErrorHandler.printMsgAndExit(e)
  |ErrorHandler.MissingDataException(e)=> ErrorHandler.printMsgAndExit(e)
  |ErrorHandler.MultipleTablesNoMergeException(e)=> ErrorHandler.printMsgAndExit(e)
  |ErrorHandler.InvalidMergeException(e)=> ErrorHandler.printMsgAndExit(e)
  |ErrorHandler.InvalidRefererException(e)=> ErrorHandler.printMsgAndExit(e)
  |ErrorHandler.PipeFunctionTypeError(e)=> ErrorHandler.printMsgAndExit(e)
  |ErrorHandler.PipeFunctionNotFound(e)=> ErrorHandler.printMsgAndExit(e)
  |ErrorHandler.TypeErrorDefaultVal(e)=> ErrorHandler.printMsgAndExit(e)
  |ErrorHandler.DataNotMatchingMetadataException(e)=> ErrorHandler.printMsgAndExit(e)
  |ErrorHandler.NoDataFoundInDatafile(e)=> ErrorHandler.printMsgAndExit(e)
  |other => ErrorHandler.printMsgAndExit("An unknown error occured")
