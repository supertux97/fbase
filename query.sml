use "scanner.sml";
use "ErrorHandler.sml";
use "operators.sml";
use "util/listUtil.sml";
use "metadataParser.sml";
use "dataParser.sml";
open Scanner; (*Also opens tok*)

structure Query = 
struct

type Query = {from:TokenAtLine list, 
              merge:TokenAtLine list,
              filter:TokenAtLine list,
              output:TokenAtLine list}
val queryTypeSeperators = ["from","merge","filter","output"]

type aliasToTablename = string*string
type mergeInfo = {table1:string, table2:string, mergeCol:string} 
type outputList = {table:string, column:string} list 

val metadataFileEnding = ".md"
val dataFileEnding = ".dat"


val tablesSep = ","
val outputFieldsSep = " | "
val outputLineSep = "~"
val outputHeaderSep = " - "

(*
==============================
======SPLITTING BY QUERY TYPE=
=============================
 *)

(*Gets the tokens associated with a part of the query. If the query-type is not present, an
 empty list is returned. The keyword used to indicate the start of the queryType
 is not included*)
fun getToksOfQueryType(toks:TokenAtLine list, queryType:string) = 
  let val toksFromTypeAndRest = ListUtil.dropWhile(toks, (fn t => TokUtil.valOfTok(t) <> queryType) )
      val ofType = if length(toksFromTypeAndRest) > 0 then 
                      ListUtil.takeWhile(tl(toksFromTypeAndRest), 
                          (fn t => not(ListUtil.member (TokUtil.valOfTok(t)) queryTypeSeperators)))
                  else []
  in 
    ofType
  end

(*Divides the tokens into the various queries. A query can be empty*)
fun splitToksIntoQueryParts(toks: TokenAtLine list):Query = 
    {from=getToksOfQueryType(toks, "from"), 
     merge=getToksOfQueryType(toks, "merge"),
     filter=getToksOfQueryType(toks, "filter"),
     output=getToksOfQueryType(toks, "output")}


(*
=======================
======FROM=============
=======================
 *)

(*Maps table-names to aliases. If no alias is given, the tablename is mapped to
itself. The function assumes the list of tokens to be
only associated with the from query-type *)
fun getTablesAndAliases(fromToks:TokenAtLine list):aliasToTablename list = 
  let fun fromQueryToAliasMapping(q:TokenAtLine list):aliasToTablename = 
        case q of 
          [x] => (case getTok(x) of 
                     Identifier(i) => (i,i)
                    |other => raise ErrorHandler.unexpectedSymbol(
                                      "Identifier",
                                       TokUtil.tokToStr(other, TokUtil.tokValAndKind),
                                       getLineNo(x)) )
         |[(Identifier(table),_), (Keyword("as"),_), (Identifier(alias),_)] => (alias, table)
         |invalid => raise ErrorHandler.malformedQuery(
                              "IDENTIFIER or IDENTIFIER \"as\" IDENTIFIER",
                               ListUtil.listToStr(
                                invalid,
                                (fn e=> TokUtil.tokToStr(getTok(e),TokUtil.tokKind))," "),
                              invalid |> hd |> getLineNo)

      (*Each sublist contains one table query and is divided using the table
      separator*)
    val tableQueries = ListUtil.splitListIntoSublists(fromToks,(fn e => TokUtil.valOfTok(e) = tablesSep))
    val tablesAndAliases = List.map fromQueryToAliasMapping tableQueries
  in tablesAndAliases
  end


(*Loades requsted data into a map. Each map entry correspnds to a single table.
The entries are a list (for all the rows) and all the list elements is a map for
all the colums of the row. Map( Table1: [Map:(field1:"test",field2:"apple",...), ...], Table2: [] )  *)
fun loadRequestedTables(requested:aliasToTablename list) =
  let fun inner( (name,table)::restRequested, map) = 
          let val metadataForTable = MetadataParser.parse(Util.fileToStr(table ^ metadataFileEnding))
              val dataForTable = Util.fileToStr(table ^ dataFileEnding) 
              val rowsForTable = Util.splitStr(dataForTable, #"\n")
              val parsedDataForTable:Tok.litteral StrMap.Map list  = DataParser.parse(metadataForTable,
                  rowsForTable, table ^ dataFileEnding)
          in inner(restRequested, StrMap.insert(map,name,parsedDataForTable) )
          end
        |inner ([], map) = map
  in inner(requested, StrMap.empty())

  end



(*
=======================
======MERGE=============
=======================
 *)

(*Extracts information about a merge from tokens. If no tokens are present, NONE
 is returned*)
fun getMergeInfo(toks:Token list, lineNo:int):mergeInfo option= 
   case toks of
    [] => NONE
   |[Identifier(t1),Keyword("and"),Identifier(t2),Keyword("using"), Identifier(mergeColumn)] => 
       SOME({table1=t1,table2=t2,mergeCol=mergeColumn})
   |other => raise ErrorHandler.malformedQuery("TABLE1 and TABLE2 using COLUMN_NAME", 
   ListUtil.listToStr(
    other,(fn
     t=>TokUtil.tokToStr(t, TokUtil.tokValAndKind))
     ," "),lineNo)

(*Returns an option of a row defined by a specific field wit ha specififc value-
 from a table*)
fun findRowById(data: litteral StrMap.Map list, fieldName:string, fieldVal:litteral) = 
    case data of 
        (row::rows) => let val found = StrMap.get(row,fieldName)
                       in case found of
                           SOME(s) => 
                                  if TokUtil.litteralEquals(s, fieldVal) then 
                                        SOME(row)
                                    else 
                                      findRowById(rows,fieldName,fieldVal)
                            |NONE => 
                                raise ErrorHandler.invalidMerge("No matching row to merge by")
                       end
        |[] => NONE


 (*T1 is matched by t2 into t3*)
fun matchRows(t1,t2,t3,mergeInfo) = 
   case t1 of 
     (t1Row::t1Rows) => 

         (case StrMap.get(t1Row,#mergeCol mergeInfo) of 
             SOME(s) => (      
                 let val t2CorrRow = findRowById(t2, #mergeCol
                  mergeInfo,s)
                 in case t2CorrRow of 
                   SOME(s) => 
                     let val merged = StrMap.addTwoMaps(s, t1Row)
                     in matchRows(tl(t1),t2,merged::t3,mergeInfo) end
                   |NONE => raise ErrorHandler.invalidMerge("Could not find corresponding row")
                 end )

             |NONE => raise ErrorHandler.invalidMerge("Could not merge tables"))

   |[] => t3

(*Puts data from mulitple tables together into one table named merged*)
fun performMerge(aliasToTableNames:aliasToTablename list, data, mergeInfoOpt:mergeInfo option) = 
  case mergeInfoOpt of
    SOME(info) => 
      let val mergeMap = StrMap.empty()
          val table1Opt = StrMap.get(data, #table1 info)
          val table2Opt = StrMap.get(data, #table2 info)
    in 
      case (table1Opt, table2Opt) of 
        (SOME(someT1),SOME(someT2)) => 
            StrMap.insert(
              mergeMap,"merged",
              matchRows(someT1,someT2,[], info))
        |_ => raise ErrorHandler.invalidMerge("could not find the requested tables for merging")
    end

  |NONE => 
    if List.length(aliasToTableNames) > 1 then
      raise ErrorHandler.multipleTablesNoMerge()
    else data



(*
=======================
======OUTPUT=============
=======================
 *)

(*Converts a single row with the requested fields into a string represetnation.
 Formatting to make it into a table like structure is applied*)
fun outputRow(row:Tok.litteral StrMap.Map,fieldsToOutput:outputList,
  lineNo:int):string = 
  case fieldsToOutput of
    (field::fields) => 
        (case StrMap.get(row,#column field) of
           SOME(s) => TokUtil.litteralToStr(s) ^ 
                      outputFieldsSep ^
                      outputRow(row,fields, lineNo)

           |NONE => raise ErrorHandler.unknownColumn(
                    #column field, #column field, lineNo))
  |[] => ""


(*Gets a string repsetantation of the requested fields. Formatting is applied*)
fun getHeader(fieldsToOutput:outputList) = 
  case fieldsToOutput of
    (x::xs) => #column x ^ outputHeaderSep ^ getHeader(xs)
    |[] => ""


(*Gets a string representation of the requested data. The data is then formatted
into a table-like format. Data is of format returned by getRequestedTables.*)
fun getOutputAndFormat(
    fieldsToOutput:outputList,
    data, lineNo:int):string = 

  let fun getOutputEachRow(fieldsToOutput,rowsIn, rowsOut:string list):string list = 
    case rowsIn of 
      (row::restRows) => 
        let val currRow = outputRow(row,fieldsToOutput,lineNo) 
        in getOutputEachRow(fieldsToOutput, restRows, currRow::rowsOut)
        end
      |[] => rowsOut
  in
    (*At this point, only one tablename is left. This is because multiple tables
     have to be merged into one*) 
    let val tableName = #table(List.nth(fieldsToOutput,0))
    in     
    case StrMap.get(data,tableName) of
       SOME(s) => 
          let val rows = getOutputEachRow(fieldsToOutput, s,[])
              val rowLengths = List.map (String.size) rows 
              val longestRow = ListUtil.max(rowLengths,ListUtil.intGreatherThan) - size(outputFieldsSep)
              val sepForLine = Util.repeatStr(outputLineSep, longestRow)
              val header = getHeader(fieldsToOutput)
              val rowsWithSepList = List.map (fn e => e ^ "\n" ^ sepForLine ^ "\n") rows
          in 
            sepForLine ^ "\n" ^ header ^ "\n" ^ sepForLine ^ "\n" ^ 
              ListUtil.listToStr(rowsWithSepList, Util.I,"") 
          end
        |NONE => raise ErrorHandler.invalidReferer(
          Util.format(
                "The table $ can no longer be refered to because it is merged" ^
                "Use the table merged instead",[tableName]))
    end
  end


(*Converts a list of tokens into a list where each elements consists of the
table and the columnname.*)
fun getOutputList(toks:Token list,lineNo:int): outputList = 
  case toks of
    Identifier(t)::Symbol(SyntaxSymbol("."))::Identifier(c)::xs => 
        {table=t, column=c} :: getOutputList(xs,lineNo)
    |Symbol(SyntaxSymbol(","))::xs => getOutputList(xs,lineNo)
    |[] => []
    |other => raise ErrorHandler.malformedQuery("output TABLENAME.COLUMNNAME,...",
                  ListUtil.listToStr(other, (fn t => TokUtil.tokToStr(t,
                  TokUtil.tokVal))," "),lineNo)


  fun getFirstLineNo(part):int = 
    if List.length(part) = 0 then ~1
    else getLineNo(List.nth(part,0))

(*Runs the query, from the query string source, to the output in string format*)
fun runQuery(q:string):string = 
  let
    val parts = q |> trimAndScan |> splitToksIntoQueryParts
    val from = #from parts
    val merge = #merge parts
    val output = #output parts
    val filter = #filter parts

    val mapped = List.map TokUtil.tokAtLineToTok merge
    val mergeInfo = getMergeInfo( (List.map TokUtil.tokAtLineToTok merge),getFirstLineNo(merge))

    val infoForRequstedTables = getTablesAndAliases(from)
    val mapOfTablesToData = loadRequestedTables(infoForRequstedTables)
    val mergedData = performMerge(infoForRequstedTables, mapOfTablesToData, mergeInfo)

    val outputToks = TokUtil.listOfTokenAtLineToToks(output)
    val outputList = getOutputList(outputToks,getFirstLineNo(output)) 
    val output = getOutputAndFormat(outputList,mergedData,getFirstLineNo(output))

  in  output
  end

val _ = print(runQuery(Util.fileToStr("q2.txt")))

end;
