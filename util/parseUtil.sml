use "tok.sml";
use "util/util.sml";
use "operators.sml";
structure ParseUtil = 
struct 
fun main a = a;

val stringSep = #"'"
(*Returns a pair of the first valid identifier and the rest of the string(starts with alpha) and is
followed by alpha or underscore*)
fun getFirstIdentifier(source:string):(string*string) =  
  let val (idSs,restSs) = Substring.splitl (fn c => Char.isAlpha(c) orelse (c = #"_")) (Util.strToSs(source))
  in ( Util.ssToStr(idSs), Util.ssToStr(restSs) )
 end

 (*Parses the first number (including decimal  and negative numbers) from a
 string.If no decimal places are found, .0 is inserted
 The eventually non-numeric  characters(excluding -) at the start is discarded
 The number and the rest of the string is returned. The characters bef*)
fun getFirstNumberFromString(str:string):(real*string) =
  let
   fun digitOrMinus c = Char.isDigit(c) orelse c = #"-"
   fun nextNumPair(str:string):(string*string) = 
     let val firstChar = Util.hdString(str) (*Allows the number to be negative*)
         val (restOfNumPart, rest) = Substring.splitl Char.isDigit (Util.strToSs(Util.tlString(str)))
     in
      (Char.toString(firstChar) ^ Util.ssToStr(restOfNumPart), Util.ssToStr(rest))
     end
 in
   if digitOrMinus(Util.hdString(str)) then
     let val (integerPart, rest) = nextNumPair str
     in
       if size(rest) > 0 andalso Util.hdString rest = #"." then
         let val (decimalPart, restAfterDecimal) = nextNumPair(Util.rmHeadOfString rest)
         in (Util.realFromString(Util.format("$.$", [integerPart, decimalPart])), restAfterDecimal)
         end
       else (Util.realFromString(Util.format("$.0",[integerPart])), rest)
     end
   else getFirstNumberFromString(Util.rmHeadOfString(str))  (*Drop first char and retry*)
  end

fun getFirstNumberFromStringAsLitteral(source:string):real = 
  let val (num, rest) = getFirstNumberFromString(source)
  in num
  end

(*Gets the string at the beginning of source, up to but not including stringSep*)
fun strFromBeginningOfStr(source:string,stringSep:char):string =  
     Util.takeWhileStr(Util.tlString(source),(fn c=>c <> stringSep))

fun padStartAndEndStr(str:string,toPad:char) = 
  Util.format("$$$",[Char.toString(toPad),str,Char.toString(toPad)])

(*Extracts the first string and the rest from another string. The extracted string is
expected to be at the start of the larger string and to end with endStr.EndStr
is not included*)
fun getFirstString(str:string, endStr:char):(string*string) = 
  let val noFirstSep = Util.strToSs(Util.rmHeadOfString(str))
      val (extracted, rest) = Substring.splitl (fn c => c <> endStr) noFirstSep
    in (extracted |> Util.ssToStr, rest |> Util.ssToStr |> Util.rmHeadOfString)
  end

(*Excpects the string to be correctly formatted*)
fun strToBool(str:string) = if str = "true" then true else false
fun isStartOfIdentifier(c:char) = Char.isAlpha(c) 
fun isStartOfwhitespace(c:char) = c = #" "
fun isStartOfString(c:char) = c = stringSep
fun isNewline(c:char) = c = #"\n"
fun isStartofSymbol(c:char,validSymbols:string list) = ListUtil.member (Char.toString(c)) validSymbols
fun isStartOfDigit(c:char) = Char.isDigit(c)
fun isStrBoolean(str:string):bool = str <>  "true" orelse str <> "false"

fun isStartOfSubtraction(c1:char,c2Opt:char option) = 
        case c2Opt of 
            SOME(c2) => c1 = #"-" andalso Char.isDigit(c2)
           |NONE => false 
fun isStartOfnegativeDigit(first:char, secondOpt:char option, thirdOpt:char option ) = 
  case (secondOpt, thirdOpt) of
      (SOME(c2), SOME(c3)) =>  first= #"(" andalso c2 = #"-" andalso Char.isDigit(c3)
     | (_, _) => false

fun getTypeOfSource(s:string):string = 
  let val firstChar = Util.hdString(s)
  in 
    if s = "true" orelse s = "false" then "boolean"
    else if isStartOfString(firstChar) then "string"
    else if isStartOfDigit(firstChar) then "integer"
    else "unknown"
  end 

end;
