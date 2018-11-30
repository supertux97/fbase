use "ErrorHandler.sml";
use "operators.sml";
use "util/listUtil.sml";
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
type pipeOperation = (litteral -> litteral)
type outputAndPipeList = {table:string, column:string, outputName:string, operations:pipeOperation list} list 

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
          [x] => (case TokUtil.getTok(x) of 
                     Identifier(i) => (i,i)
                    |other => raise ErrorHandler.unexpectedSymbol(
                                      "Identifier",
                                       TokUtil.tokToStr(other, TokUtil.tokValAndKind),
                                       TokUtil.getLineNo(x)) )
         |[(Identifier(table),_), (Keyword("as"),_), (Identifier(alias),_)] => (alias, table)
         |invalid => raise ErrorHandler.malformedQuery(
                              "IDENTIFIER or IDENTIFIER \"as\" IDENTIFIER",
                               ListUtil.listToStr(
                                invalid,
                                (fn e=> TokUtil.tokToStr(TokUtil.getTok(e),TokUtil.tokKind))," "),
                              invalid |> hd |> TokUtil.getLineNo)

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
          let val metadataFile = table ^ metadataFileEnding
               val metadataForTable =
                 MetadataParser.parse(Util.fileToStr(metadataFile),metadataFile)
              val dataForTable = Util.fileToStr(table ^ dataFileEnding) 
              val rowsForTable = Util.splitStr(dataForTable, #"\n")
              val parsedDataForTable:litteral StrMap.Map list  = DataParser.parse(metadataForTable,
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
======PIPE_FUNCTIONS===
=======================
 *)
fun raiseTypeError(found:Tok.litteral, required:string) =
  raise ErrorHandler.pipeFunctionTypeError(
            TokUtil.getLitteralKind(found),required)

fun transformStr(t:Tok.litteral, eachCharFunc: (char -> char)) =
 case t of
  Tok.String(s) =>
    let val chars = String.explode(s)
          val upperCaseChars = List.map eachCharFunc chars
      in
       Tok.String(String.implode(upperCaseChars))
      end
  |other => raiseTypeError(other,"string")

fun toUpper(t:Tok.litteral) =
  transformStr(t, Char.toUpper)

fun toLower(t:Tok.litteral) =
 transformStr(t, Char.toLower)

fun backwards(t:Tok.litteral) = 
  let fun performBackwards(str:string):string = 
    str |> String.explode |> rev |> String.implode
  in
  case t of 
     Tok.String(s) => Tok.String(performBackwards(s))
    |other => raiseTypeError(other,"string")
  end

(*Creates a string consisting of the number with a space for each third digit
 Ex: 555000 -> 1 000*)
fun numSep(n: Tok.litteral) =
  let fun performSep(c::chars, currDigit):char list =
           (case currDigit mod 3 of
               0 => c :: #" " :: performSep(chars, currDigit + 1)
              |n => c :: performSep(chars,currDigit + 1))

         |performSep([], currDigit) = []
    in
      case n of
          Tok.Number(n) =>
            let
              val numbers = Real.toString(n)
              val numbersChars = String.explode(numbers)
              val revNumbers = rev(numbersChars)

              val decimalPart = #"." :: ListUtil.takeWhile(revNumbers,(fn c => c <> #"."))
              val numberPart = tl(ListUtil.dropWhile(revNumbers,(fn c => c <> #".")))
              val numSeperatedRev = performSep(numberPart,1)
              val numSeperated = rev(numSeperatedRev)
          in
            Tok.String(String.implode(numSeperated @ decimalPart))
          end
         |other => raiseTypeError(other, "number")
   end

(*Makes the first character upper-case*)
fun capitalized(t:Tok.litteral) =
  case t of
    Tok.String(s) =>
        let val upper = s |> Util.hdString |> Char.toUpper |> Char.toString
            val rest = String.substring(s, 1, size(s) -1)
        in Tok.String(upper ^ rest)
        end
   |other => raiseTypeError(other,"string")

fun trim(s:Tok.litteral) = 
 case s of 
  Tok.String(s) => Tok.String(s |> Util.rmSpaceAtStartOrEnd |> Util.rmMultipleSpace) 
  |other => raiseTypeError(other, "string")

(*
=======================
======OUTPUT=============
=======================
 *)

fun applyOperations(lit:litteral,operations:pipeOperation list):litteral = 
  case operations of 
    (x::xs) => applyOperations( (x lit), xs) 
    |[] => lit 

fun removeDelimiters(lit:Tok.litteral) = 
  case lit of 
    Tok.String(s) => Tok.String(s |> Util.rmHeadOfString |> Util.rmTailOfString)
    |other => other

fun applyDelimiters(lit:Tok.litteral) = 
  let val strDelim = Char.toString(Scanner.stringSep)
  in case lit of 
    Tok.String(s) => Tok.String(Util.format("$$$", [strDelim,s,strDelim]))
   |other => other
  end

(*Converts a single row with the requested fields into a string represetnation.
 Formatting to make it into a table like structure is applied*)

fun outputRow(row:litteral StrMap.Map,fieldsToOutput:outputAndPipeList,
  lineNo:int):string list = 
  case fieldsToOutput of
    (field::fields) => 
        (case StrMap.get(row, #column field) of
              (*Delimiters: ' for strings, is removed when using in
                ipe-functions to avoid problems*)
           SOME(s) => let val delimitersRemoved = removeDelimiters(s) 
                          val cellValAppliedOperations =
                            applyOperations(delimitersRemoved,
                                 rev(#operations field))
                          val delimitersApplied =
                            applyDelimiters(cellValAppliedOperations)
                      in
                      TokUtil.litteralToStr(delimitersApplied) ::
                      outputRow(row,fields, lineNo)
                      end

           |NONE => raise ErrorHandler.unknownColumn(
                    #column field, #table field, lineNo))
  |[] => [""]


(*Gets a string repsetantation of the requested fields. Formatting is applied*)
fun getHeader(fieldsToOutput:outputAndPipeList,sizeForCol:int list) = 
  case (fieldsToOutput,sizeForCol) of
    (x::xs,s::ss) => let val colName = #outputName x
                         val lengthMissing = s - size(colName)
                         val padded = colName ^ Util.repeatStr(" ", lengthMissing)
                     in padded ^ outputHeaderSep ^ getHeader(xs,ss)
                     end
    |([],[]) => ""

(*Finds the length of the largest element in each column. This is useful for
padding the colums to that they have a common width. The output is a list where
 each elemnent correspnds to each column*)
fun getMaxColLenghts(rows:string list list):int list = 
  let fun maxColLenghtsForRow(cols:string list,longest:int list,idx:int):int list = 
          case cols of
            c::cs => 
                 let val lenCol = size(c)
                     val currLongest = List.nth(longest,idx)
                 in if lenCol > currLongest then
                      maxColLenghtsForRow(
                        cs,ListUtil.replaceElemAtIndex(longest,idx,lenCol),idx+ 1)
                    else 
                      maxColLenghtsForRow(cs,longest,idx+1)
                 end
            |[] => longest
      fun maxForColsInner(longest:int list,rows) = 
        case rows of 
          (r::rs) => let val longestNow = maxColLenghtsForRow(r,longest,0)
                         in maxForColsInner(longestNow,rs)
                     end
          |[] => longest
    in 
        maxForColsInner(ListUtil.fillWith(0,List.length(List.nth(rows,0))),rows)
  end

(*Ensures that the width of each row is matching the width correspnding column in
  lengthOfColums. The difference is replaced with spaces. It is expected that
  the elems lengthOfColums is never smaller than any of the elenents in
    colsForRow*)
fun applyPaddingAndFormattingForRow(colsForRow:string list, lengthOfColums:int list):string = 
  let fun applyInner(cols,idx) = 
    case cols of
      c::cs => let val missing = List.nth(lengthOfColums,idx) - size(c)
                            val padded = c ^ (Util.repeatStr(" ", missing))
                        in 
                          padded ^ outputFieldsSep ^
                          applyInner(cs,idx + 1)
                        end
     |[]  => ""
  in 
    applyInner(colsForRow,0)
  end
  handle Subscript => raise Match

(*Gets a string representation of the requested data. The data is then formatted
into a table-like format. Data is of format returned by getRequestedTables.*)
fun getOutputAndFormat(
    fieldsToOutput:outputAndPipeList,
    data, lineNo:int):string = 

  let fun getOutputEachRow(fieldsToOutput,rowsIn, rowsOut:string list list):string
  list list = 
    case rowsIn of 
      (row::restRows) => 
        let val currRow = outputRow(row,fieldsToOutput,lineNo) 
        in getOutputEachRow(fieldsToOutput, restRows, currRow :: rowsOut)
        end
      |[] => rowsOut

  in
    (*At this point, only one tablename is left. This is because multiple tables
     have to be merged into one*) 
    let val tableName = #table(List.nth(fieldsToOutput,0))
    in     
    case StrMap.get(data,tableName) of
       SOME(s) => 
          let val listOfFieldsOfRows:string list list = getOutputEachRow(fieldsToOutput, s,[])
              val listOfColSizes:int list = getMaxColLenghts(listOfFieldsOfRows) 
              val rows:string list = List.map (
                    fn r => applyPaddingAndFormattingForRow(r,listOfColSizes))
                    listOfFieldsOfRows
              val rowLengths = List.map (String.size) rows 
              val longestRow = ListUtil.max(rowLengths,ListUtil.intGreatherThan) - size(outputFieldsSep)
              val sepForLine = Util.repeatStr(outputLineSep, longestRow)
              val header = getHeader(fieldsToOutput,listOfColSizes)
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

fun getPipeFunctionFromName(name:string) = 
  if name = Scanner.pfToLowerCase then toLower
  else if name = Scanner.pfToUpperCase then toUpper 
  else if name = Scanner.pfCapitalized then capitalized
  else if name = Scanner.pfNumSep then numSep
  else if name = Scanner.pfBackwards then backwards
  else if name = Scanner.pfTrim then trim
  else raise ErrorHandler.pipeFunctionNotFound(name)

(*Gets the list of operations to be performed for a single column and the tokens
 after the operations*)
fun getPipeOpeations(toks:Token list):(pipeOperation list * Token list) = 
  let fun getOperations(operations,toks) = 
        case toks of
         PipeFunction(pf)::ts =>
           getOperations(getPipeFunctionFromName(pf)::operations, ts)
        |Symbol(SyntaxSymbol("->"))::ts => getOperations(operations,ts)
        |Symbol(SyntaxSymbol(","))::ts => (operations, ts)
        |[] => (operations, toks)
        |other::ts => raise
        ErrorHandler.pipeFunctionNotFound(TokUtil.tokToStr(other,
        TokUtil.tokVal))
  in
  case toks of
    (Symbol(SyntaxSymbol("->"))::ts) => getOperations([],ts)
    |[] => ([], [])
    |other => ([],toks) 
  end
(*Converts a list of tokens into a list where each elements consists of the
table and the columnname as well as a list of the operations to be performed on
 the column.*)
fun getOutputAndPipeList(toks:Token list,lineNo:int): outputAndPipeList= 
  case toks of
     (*query for table*)
     Identifier(t)::Symbol(SyntaxSymbol("."))::Identifier(c)::xs => (
          case xs of 
            (*Custom col name*)
             Keyword("named")::Identifier(n)::xsNamed=> 
              let val (pipeOperations, rest) = getPipeOpeations(xsNamed) 
              in
                {table=t, column=c, outputName=n, operations=pipeOperations} ::
                getOutputAndPipeList(rest,lineNo)
              end

            (*Default name*)
            |other => 
              let val (pipeOperations, rest) = getPipeOpeations(xs) 
              in
                {table=t, column=c, outputName=c, operations=pipeOperations} ::
                getOutputAndPipeList(rest,lineNo)
              end )

     (*query for table finished*)
    |Symbol(SyntaxSymbol(","))::xs => getOutputAndPipeList(xs,lineNo)
    |[] => []
    |other => raise ErrorHandler.malformedQuery("output TABLENAME.COLUMNNAME,...",
                  ListUtil.listToStr(other, (fn t => TokUtil.tokToStr(t,
                  TokUtil.tokVal))," "),lineNo)


  fun getFirstLineNo(part):int = 
    if List.length(part) = 0 then ~1
    else TokUtil.getLineNo(List.nth(part,0))

(*
=============
===PIPELINING
=============
 *)


(*Runs the query, from the query string source, to the output in string format*)
fun runQuery(q:string):string = 
  let
    val queryParts = q |> trimAndScan |> splitToksIntoQueryParts
    val from = #from queryParts
    val merge = #merge queryParts
    val output = #output queryParts
    val filter = #filter queryParts
    
    (*Gets information about what data should be outputted and how it should be outputted and
    transformed*) 
    val mergeInfo = getMergeInfo( (List.map TokUtil.tokAtLineToTok merge),getFirstLineNo(merge))
    val outputToks = TokUtil.listOfTokenAtLineToToks(output)
    val tablesAndAliasesRequested = getTablesAndAliases(from)
    val outputAndPipeList = getOutputAndPipeList(outputToks,getFirstLineNo(output)) 

    (*Gets data*)
    val mapOfTablesToData = loadRequestedTables(tablesAndAliasesRequested)
    val mergedData = performMerge(tablesAndAliasesRequested, mapOfTablesToData, mergeInfo)
    (*Outputs data*)
    val output = getOutputAndFormat(outputAndPipeList,mergedData,getFirstLineNo(output))

  in  output
  end

val _ = print(runQuery(Util.fileToStr("q2.txt")))

end;
