use "scanner.sml";
use "ErrorHandler.sml";
use "operators.sml";
use "listUtil.sml";
open TokUtil;
open Scanner;

structure Query = 
struct
val fromTableSep = ","
type Query = {from:TokenAtLine list, 
              merge:TokenAtLine list option,
              filter:TokenAtLine list option,
              output:TokenAtLine list}
val queryTypeSeperators = ["from","merge","filter","output"]
type aliasToTablename = string*string

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

(*Maps table-names to aliases. If no alias is given, the tablename is mapped to
itself. The function assumes the list of tokens to be
only associated with the from query-type *)
fun getTablesAndAliases(fromToks:TokenAtLine list) = 
  let fun singleQueryToMapping(q:TokenAtLine list):aliasToTablename = 
        case q of 
          [x] => (case getTok(x) of 
                     Identifier(i) => (i,i)
                    |other => raise ErrorHandler.unexpectedSymbol("Identifier", tokToStr(other, tokValAndKind), getLineNo(x)) )
         |[ (Identifier(i),_),(Keyword("as"),_), (Identifier(j),_)] => (j,i)
         |invalid => raise ErrorHandler.malformedQuery("IDENTIFIER or IDENTIFIER \"as\" IDENTIFIER",
               ListUtil.listToStr(invalid,(fn e=> tokToStr(getTok(e),tokKind))," "),invalid |> hd |> getLineNo)
 
      (*Each sublist contains one table query*)
    val tableQueries = ListUtil.splitListIntoSublists(fromToks,(fn e => valOfTok(e) = fromTableSep))
    val tablesAndAliases = List.map singleQueryToMapping tableQueries
  in tablesAndAliases
  end

(*Divides the tokens into the various queries. A query can be empty*)
fun splitToksIntoQueryParts(toks: TokenAtLine list) = 
    {from=getToksOfQueryType(toks, "from"), 
     merge=getToksOfQueryType(toks, "merge"),
     filter=getToksOfQueryType(toks, "filter"),
     output=getToksOfQueryType(toks, "output")}

  val q = "from Employee as E, Students as S, Salary merge E and S using id filter id = 32"  
  val toks = trimAndScan(q)
  val parts = splitToksIntoQueryParts(toks)
  val from = #from parts
  val tal = getTablesAndAliases(from)
  val _ = print(ListUtil.listToStr(tal,(fn tal=>Util.format("$->$",[#1 tal, #2 tal])), "\n"))
end;

