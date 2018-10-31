use "listUtil.sml";
fun main a = a
structure Util = 
struct
exception NotEnoughListElements of string;
exception ConvesionFormatException of string
fun ssToStr(s:substring) = Substring.string s

fun rmHeadOfString(str:string):string = String.substring(str,1,size(str)-1)
fun rmTailOfString(str:string):string = String.substring(str,0,size(str)-2)

fun strToSs(s:string) = Substring.full s

fun tlString(str:string):string = String.substring(str,1,size(str) -1)
fun tlStringOpt(str:string):string option = 
  if size(str) = 0 then NONE 
  else SOME(String.substring(str,1,size(str) -1))
fun hdString(str:string):char = String.sub(str,0)
fun hdStringOpt(str:string):char option = 
  if size(str) = 0 then NONE
  else SOME(String.sub(str,0))
fun println(msg:string) = print(msg ^ "\n")
fun op $ (n:int) = Int.toString(n)

fun I(a) = a

fun splitStr(str:string) = 
  (hdString(str),tlString(str))

(*Creates a tring, character by character as long as the pred for the curent
char is true*)
fun takeWhileStr(str:string, pred:(char->bool)) = 
  case (hdStringOpt(str), tlStringOpt(str)) of 
       (SOME(x),SOME(xs)) => if pred(x) then Char.toString(x) ^ takeWhileStr(xs, pred)
                       else ""
      |(_,_) => ""

(*Removes characters from a string as long as pred is true.
 Returns: The Resulting string *)
fun dropWhileStr(str:string, pred:(char->bool)) = 
  case (hdStringOpt(str), tlStringOpt(str)) of 
       (SOME(x),SOME(xs)) => if pred(x) then dropWhileStr(xs, pred)
                       else Char.toString(x) ^ xs
      |(_,_) => ""

(*Example usage: printf("Name: $1 Age: $2", ["Name", $15]*)
fun format(str:string, vals:string list) =
  let
    val chars = String.explode(str)
    val (hdChar,tlChar) = (hd(chars),tl(chars))
  in
    case vals of 
        [] => str 
       |_ => 
          case (hdChar, tlChar) of
                (#"$", nil) => if vals = nil then raise NotEnoughListElements("Missing list elements")
                           else String.substring(str,1,size(str) -1) ^ hd vals
               |(_,nil) => str
               |(#"$",r) =>
                   if(vals = nil) then raise NotEnoughListElements("Missing list elements")
                   else hd vals ^ format(ListUtil.listToStr(r,Char.toString,""), tl vals)
               |(f,r) => Char.toString(f) ^ format(ListUtil.listToStr(r,Char.toString,""), vals)
  end;

fun fib 0 = 1
   |fib 1 = 1
   |fib n = fib(n-1) + fib(n-2)

fun fibTail(n)= 
  let fun fibInner(n1, n2, remain) = 
      if remain <= 1 then n2  
      else fibInner(n2, n1 + n2, remain-1)
  in 
    fibInner(1, 1, n)
  end

fun formatln(str:string, vals:string list) = 
  format(str,vals) ^ "\n"

(*Removes the firsst occurence of a character in a string. If the character is
not found, the original string is returned untouched*)
fun rmFirstCharMatchOfString(c:char,str:string) = 
    let val (beforeMatch, afterMatch) = Substring.splitl (fn curr => curr <> c) (strToSs(str))
    in ssToStr beforeMatch ^ rmHeadOfString(ssToStr(afterMatch))
  end

(*A version which uses the Real-libarys function for parsing, but resturns the
real or raises an exeption instead of returning a option*)
fun realFromString(str:string):real =
 case Real.fromString(str) of
     SOME(r) => r
   | NONE => raise ConvesionFormatException(format("Could not parse the string intoa real", [str]))

 (*Parses the first number (including decimal  and negative numbers) from a
 string.If no decimal places are found, .0 is inserted
 The eventually non-numeric  characters(excluding -) at the start is discarded
 The number and the rest of the string is returned. The characters bef*)
fun getFirstNumberFromString(str:string):(real*string) =
  let
   fun digitOrMinus c = Char.isDigit(c) orelse c = #"-"
   fun nextNumPair(str:string):(string*string) = 
     let val firstChar = hdString(str) (*Allows the number to be negative*)
         val (restOfNumPart, rest) = Substring.splitl Char.isDigit (strToSs(tlString(str)))
     in
      (Char.toString(firstChar) ^ ssToStr(restOfNumPart), ssToStr(rest))
     end
 in
   if digitOrMinus(hdString(str)) then
     let val (integerPart, rest) = nextNumPair str
     in
       if size(rest) > 0 andalso hdString rest = #"." then
         let val (decimalPart, restAfterDecimal) = nextNumPair( rmHeadOfString rest)
         in (realFromString(format("$.$", [integerPart, decimalPart])), restAfterDecimal)
         end
       else (realFromString(format("$.0",[integerPart])), rest)
     end
   else getFirstNumberFromString(rmHeadOfString(str))  (*Drop first char and retry*)
 end

fun splitStrByNewline(str:string):string list =
  let val substr = Substring.full(str)
  in
    map (fn e => Substring.string e) (Substring.fields (fn c => c = #"\n") substr )
  end

fun fileToStr(fname:string):string =
  TextIO.inputAll(TextIO.openIn(fname))

fun getCharAtIndex(str:string, index:int):char option =
  let
    fun getCharAtIndex(str:string, index:int, currIndex:int):char option =
      if(currIndex = size(str)) then NONE
      else if(currIndex = index) then SOME(String.sub(str, index))
      else getCharAtIndex(str, index, currIndex +1)
  in
    getCharAtIndex(str, index, 0)
  end
end;
