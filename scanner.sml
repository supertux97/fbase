use "util/parseUtil.sml";
use "util/listUtil.sml";
use "utest.sml"; 
use "ErrorHandler.sml";
use "util/tokUtil.sml";

open Tok

(*Structure for tasks relating to scanning the query-splitting the source into
appropriate parts. The scanner also removes comments and excess spaces*)
structure Scanner = 
struct
val keywords = ["from","filter","using","and","or",
              "merge","insert","rows","into","remove","as",
              "where", "set","create","table","with",
              "colums","of","default","string","boolean","number",
              "output"]

val pipeOperator = "->"
val stringSep = #"'"
val commentSymbol = #"#"

val pfToLowerCase = "lower"
val pfToUpperCase = "upper"
val pfNumSep = "numSep"
val pfTrim = "trim"
val pfCapitalized = "capitalized"
val pfBackwards = "backwards"
val pipeFunctions = [pfToLowerCase, pfToUpperCase, pfNumSep, pfCapitalized,
 pfBackwards,pfTrim]

val functions = ["toUpper","toLower","oneof","noneof"]

val operatorsLenOne = ["+","-","*","/"]
val operators = operatorsLenOne

val predicateOperatorsLenOne = ["=","<",">"]
val predicateOperatorsLenTwo = ["<=",">=", "!="]
val predicateOperators = predicateOperatorsLenOne @ predicateOperatorsLenTwo

val syntaxSymbolsLenOne = [".",",",":","{","}","(", ")","*","|",Char.toString(stringSep)] 
val syntaxSymbolsLenTwo = [pipeOperator]
val syntaxSymbols = syntaxSymbolsLenOne @ syntaxSymbolsLenTwo 

val oneLenOperators = operatorsLenOne @ predicateOperatorsLenOne @ syntaxSymbolsLenOne
val twoLenOperators = predicateOperatorsLenTwo @ syntaxSymbolsLenTwo 

val firstCharTwoLenOperators = map (fn e => Char.toString(String.sub(e,0))) twoLenOperators
val validSymbols = predicateOperators @ syntaxSymbols @ operators @ firstCharTwoLenOperators

(*Removes all comments from a list of lines. A comment has effects from where it
 is found, to the rest of the line.*)
fun rmComments(lines: string list):string list =
  case lines of
    (x::xs) =>
    let
      val substr = Util.strToSs(x)
      val (noComment, comment) = Substring.splitl (fn c => c <> commentSymbol) substr
    in
      Util.ssToStr(noComment) :: rmComments(xs)
    end
    |[] => []


(*
==============================
======TOKENIZING/SCANNING=====
=============================
 *)

  (*Get the first operator from a string and the rest of the string. The
  function does also handle opeators of length > 2
   Unwanted chars are removed unitil a valid symbol occurs.

   The function will first read one symbol and then the next (if any)
   A two-part sybol is retuned if the two chars is part of two-symbol list, the
   one-part if just the first is part of one-part list or an emptry string if
   none of the above is true*)
  fun getOperatorFromString(str:string,l1:string list, l2:string list):(string*string) = 
      if size(str) = 0 then ("","")
      else if size(str) >= 2 then
        let val firstTwoChars = String.substring(str,0,2)
            val firstChar = String.substring(str,0,1)
        in 
          if ListUtil.member firstTwoChars l2 then (firstTwoChars, String.substring(str, 2, size(str) -2)) 
          else if ListUtil.member firstChar l1 then (firstChar, String.substring(str,1, size(str) -1))
          else ("",str)
        end
      else if ListUtil.member (Char.toString(Util.hdString(str))) l1 
        then (Char.toString(Util.hdString str ), String.substring(str,1, size(str) -1))
      else getOperatorFromString(Util.rmHeadOfString str,l1,l2)

(*Determines which kind of tok the passed s belongs to. A collection of lists
with reserved symbols are used to determine this. If not found in any of the
lists, the string is determined to be an identifier*)
fun determineTokTypeForReserved(s:string):Token =
  let val tComparator = ListUtil.member(s)
  in
    if tComparator keywords then Keyword(s)
    else if tComparator functions then Function(s)
    else if tComparator pipeFunctions then PipeFunction(s)
    else if tComparator operators then Symbol(Operator(s))
    else if tComparator predicateOperators then Symbol(PredicateOperator(s))
    else if tComparator syntaxSymbols then Symbol(SyntaxSymbol(s))
    else Identifier(s)
  end

fun charAtIdxOption(str,idx) = 
  if idx < size(str) then SOME(String.sub(str,idx))
  else NONE

(*Splitts a string of the query into different tokens. Excess whitespace is
ignored. If an unknwon symbol is found, an exeption will be trown*)
fun scan(str:string, lineNo:int):TokenAtLine list =
  let
    val firstChar = String.sub(str, 0) 
    val secondCharOpt = charAtIdxOption(str,1) 
    val thirdCharOpt = charAtIdxOption(str, 2)  
    val subString = Util.strToSs(str) 
    
  in 
    if ParseUtil.isStartOfIdentifier(firstChar) then
      let val (firstId, rest) = ParseUtil.getFirstIdentifier(str)
          val token = determineTokTypeForReserved(firstId)
      in (token, lineNo) :: scan(rest, lineNo)
      end

    else if ParseUtil.isStartOfwhitespace(firstChar) then scan(Util.rmHeadOfString(str), lineNo)

    else if ParseUtil.isStartOfString(firstChar) then
      let val (firstStr, rest) = ParseUtil.getFirstString(str, stringSep)  
      in  (Litteral(String(firstStr)), lineNo) ::
            scan(rest,lineNo)
      end
    else if ParseUtil.isStartOfSubtraction(firstChar, secondCharOpt) then 
      let val (number,rest) = ParseUtil.getFirstNumberFromString(Util.rmHeadOfString(str))
      in  (determineTokTypeForReserved("+"),lineNo) ::
           (Litteral(Number(#1(ParseUtil.getFirstNumberFromString("-" ^
           Real.toString(number))))),lineNo):: scan(rest,lineNo)
      end
    else if ParseUtil.isStartOfDigit(firstChar) then
      let val (number, rest) = ParseUtil.getFirstNumberFromString(str)
      in ( Litteral(Number(number)), lineNo) :: scan(rest, lineNo)
      end

    else if ParseUtil.isStartOfnegativeDigit(firstChar,secondCharOpt ,thirdCharOpt) then 
      let val parenRemoved = Util.rmFirstCharMatchOfString(#")", Util.rmHeadOfString(str))
          val (number, rest) = ParseUtil.getFirstNumberFromString(parenRemoved)
      in ( Litteral(Number(number)), lineNo) :: scan(rest, lineNo)
      end

    else if ParseUtil.isNewline(firstChar) then scan(Util.rmHeadOfString(str), lineNo +1)

    else if ParseUtil.isStartofSymbol(firstChar,validSymbols) then 
        let val (symbol, rest) = getOperatorFromString(str, oneLenOperators, twoLenOperators)
        in (determineTokTypeForReserved(symbol),lineNo) :: scan(rest, lineNo)
        end

    else 
      raise ErrorHandler.noSuchSymbol(Char.toString(firstChar),lineNo)
  end
  handle Subscript => []


(*Removes irelevant information and scans the supplied query*)
fun trimAndScan(str:string):TokenAtLine list =
  let
    val noComments = rmComments(Util.splitStrByNewline(str))
  in
    scan(ListUtil.listToStr(noComments, Util.I," "),1)
  end

end;
