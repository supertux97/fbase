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
type aliasToTablename = string*string

fun valOfTok(t:TokenAtLine) = tokToStr(getTok(t), tokVal)

(*Gets the tokens associated with a query. If the query-type is not present, an
 empty list is returned. The keyword used to indicate the start of the queryType
 is not included*)
fun getToksOfQueryType(toks:TokenAtLine list, queryType:string) = 
  let val toksFromTypeAndRest = ListUtil.dropWhile(toks, (fn t => valOfTok(t) <> queryType) )
      val ofType = if length(toksFromTypeAndRest) > 0 then 
                      ListUtil.takeWhile(tl(toksFromTypeAndRest), (fn t => not(ListUtil.member (valOfTok(t)) queryTypeSeperators)))
                  else []
  in 
    ofType
  end

(*Maps table-names to aliases. The function assumes the list of tokens to be
only associated with the from query-type *)
fun getTablesAndAliases(fromToks:TokenAtLine list):aliasToTablename list = 
 case fromToks of
     (x::xs) => case getTok(fromToks) of 
                     Identifier(id) => (id, getTablesAndAliases)
  
(*Divides the tokens into the various queries. A query can be empty*)
fun splitToksIntoQueryParts(toks: TokenAtLine list) = 
    {from=getToksOfQueryType(toks, "from"), 
     merge=getToksOfQueryType(toks, "merge"),
     filter=getToksOfQueryType(toks, "filter"),
     output=getToksOfQueryType(toks, "output")}

  val q = "from Employee as E, Students as S merge E and S using id filter id = 32"  
  val toks = trimAndScan(q)
  val _ = print(tokListToStr(toks))
  val parts = splitToksIntoQueryParts(toks)
  val part = #output parts
end;

