use "tok.sml";
use "util/util.sml";
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
fun getFirstStringAsLitteral(source:string,stringSep:char):string =  
     Util.takeWhileStr(Util.tlString(source),(fn c=>c <> stringSep))

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
end;
