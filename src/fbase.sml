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
