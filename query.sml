use "scanner.sml";
use "ErrorHandler.sml";
use "operators.sml";
use "util/listUtil.sml";
use "metadataParser.sml";
use "dataParser.sml";
use "map/map.sml";
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

val metadataFileEnding = ".md"
val dataFileEnding = ".dat"

(*Gets the tokens associated with a query. If the query-type is not present, an
 empty list is returned. The keyword used to indicate the start of the queryType
 is not included*)
fun getToksOfQueryType(toks:TokenAtLine list, queryType:string) = 
  let val toksFromTypeAndRest = ListUtil.dropWhile(toks, (fn t => TokUtil.valOfTok(t) <> queryType) )
      val ofType = if length(toksFromTypeAndRest) > 0 then 
                      ListUtil.takeWhile(tl(toksFromTypeAndRest), (fn t => not(ListUtil.member (valOfTok(t)) queryTypeSeperators)))
                  else []
  in 
    ofType
  end

(*Maps table-names to aliases. If no alias is given, the tablename is mapped to
itself. The function assumes the list of tokens to be
only associated with the from query-type *)
fun getTablesAndAliases(fromToks:TokenAtLine list):aliasToTablename list = 
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

(*Loades requsted data into a map. Each map entry has a key of the table name,
  and the value is a list of the maps. Each of those maps inside the list
    corespnds to a row.
Map( Table1: [Map:(field1:"test",field2:"apple",...), ...], Table2: [] )  *)

fun loadRequestedTables(requested:aliasToTablename list) =
  let fun inner((name,table)::restRequested, map) = 
          let val metadataForTable = MetadataParser.parse(Util.fileToStr(table ^ metadataFileEnding))
              val dataForTable = Util.fileToStr(table ^ dataFileEnding) 
              val rowsForTable = Util.splitStr(dataForTable, #"\n")
              val parsedDataForTable = DataParser.parse(metadataForTable,
              rowsForTable, table ^ dataFileEnding)
          in inner(restRequested, StrMap.insert(map,name,parsedDataForTable) )
          end
        |inner ([], map) = map
  in inner(requested, StrMap.empty())
  end
  handle IO => ErrorHandler.noSuchTable("")  

fun runQuery(q:string):string = 
  let
  val parts = q |> trimAndScan |> splitToksIntoQueryParts
  val from = #from parts
  val infoForRequstedTables = getTablesAndAliases(from)
  val mapOfTablesToData = loadRequestedTables(infoForRequstedTables)
    in "test" 
  end

  val t = runQuery(Util.fileToStr("query.txt"))
end;
