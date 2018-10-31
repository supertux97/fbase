use "scanner.sml";
use "ErrorHandler.sml";
use "operators.sml";
open Scanner;
structure Query = 
struct

type Query = {from:TokenAtLine list, 
              merge:TokenAtLine list option,
              filter:TokenAtLine list option,
              output:TokenAtLine list}
val queryTypeSeperators = ["from","merge","filter","output"]
(*Splits a list of tokens into lists of the different kinds of query types
defined in Query. The function does a naive division by adding to the list
unitil another keyword separating the different parts occurs.*)
 (*case toks of 
     [] => raise ErrorHandler.emptyQuery()
    |(x::xs) => 
        case Scanner.getTok(x) of 
          Keyword(k) => 
            case k of
                 "from" => {from=}
         |other =>
             raise
             ErrorHandler.unexpectedSymbol("Keyword",tokToStrWithType(other),Scanner.getLineNo(x))
  *)
fun getToksOfQueryType(toks:TokenAtLine list, queryType:string) = 
  let val startAtType = ListUtil.dropWhile(toks,
           (fn t => tokToStr(t, tokVal) <> queryType) )
      val ofType = ListUtil.takeWhile(startAtType, (fn t => not(Util.member t queryTypeSeperators)))
  in 
    ofType
  end

(*fun splitToksIntoQueryParts(toks: TokenAtLine list) = 
    {from:TokenAtLine list = getToksOfQueryType("from")} *)


  val q = "from Empployees as E merge e jrejjr jrjrj filter bbbb bbb rjrjrj output ajaj jejej jejej jejej"  
  val toks = trimAndScan(q)
  val fromToks = getToksOfQueryType(toks,"from")
  val _ = print(Util.listToStr(fromToks, Int.toString, " "))

end;

