fun main s = print("main")

exception NotEnoughListElements of string;
exception ConvesionFormatException of string
fun ssToStr(s:substring) = Substring.string s

fun rmHeadOfString(str:string):string = String.substring(str,1,size(str)-1)
fun rmTailOfString(str:string):string = String.substring(str,0,size(str)-2)

fun strToSs(s:string) = Substring.full s
fun listToStr ([], toStr:('a->string),sep:string):string = ""
  | listToStr(x::xs, toStr:('a->string),sep:string) = toStr x ^ sep ^ listToStr(xs,toStr,sep)

fun dropN([], n:int) = []
  | dropN(l,0) = l
  | dropN(x::xs,n) = dropN(xs,n-1) 

fun hdString(str:string):char = String.sub(str,0)

fun op $ (n:int) = Int.toString(n)

fun I(a) = a

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
                   else hd vals ^ format(listToStr(r,Char.toString,""), tl vals)
               |(f,r) => Char.toString(f) ^ format(listToStr(r,Char.toString,""), vals)
  end;

fun formatln(str:string, vals:string list) = 
  format(str,vals) ^ "\n"

(*Removes the firsst occurence of a character in a string. If the character is
not found, the original string is returned untouched*)
fun rmFirstCharMatchOfString(c:char,str:string) = 
    let val (beforeMatch, afterMatch) = Substring.splitl (fn curr => curr <> c) (strToSs(str))
    in ssToStr beforeMatch ^ rmHeadOfString(ssToStr(afterMatch))
  end

fun tlString(str:string):string = String.substring(str,1,size(str) -1)
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

fun minOfIntList(l: int list):(int*int) =
  let
  fun minOfIntList(l: int list, i:int, min:int, minIdx:int) =
    if i = List.length(l) then (min, minIdx)
    else
      let val curr = List.nth(l,i)
      in
        if(curr < min) then minOfIntList(l,i+1,curr,i)
        else minOfIntList(l,i+1,min,minIdx)
      end
    in minOfIntList(l,0,hd l,0)
  end

fun removeElemAtIndex(l:'a list,i:int) =
let
  fun removeElemAtIndex([], i:int, curr:int) = []
    | removeElemAtIndex(x::xs, i, curr) =
      if curr = i then xs
      else x:: removeElemAtIndex(xs, i, curr+1)
  in
    removeElemAtIndex(l,i,0)
  end

(*Returns a list consisting of just the first n characters of every element in a
string list.
 Example: ["abc","cake","def"] -> ["a","c","d"]
 Raises: Subscript(from String.sub) if there is elements
fun firstNCharsOfStringList(l:string list,n:int) = 
  case l of
      [] => []
     |(x::xs) => String.substring(x,n) :: firstCharsOfStringList(xs) *)

fun fileToStr(fname:string):string =
  TextIO.inputAll(TextIO.openIn(fname))


fun insertionSort [] = []
  | insertionSort (l:int list) =
    let
      val (minElem, minIdx) = minOfIntList(l)
    in minElem :: insertionSort(removeElemAtIndex(l,minIdx))
    end


(*adapted with variations from "ML for the working programmer"*)
fun member(x) l = List.exists (fn elem => elem = x) l

(*Adopted from "Ml for the working programmer"*)
infix memberOf
fun (x memberOf []) = false
| (x memberOf (e::r)) = (x=e) orelse (x memberOf r)

fun getCharAtIndex(str:string, index:int):char option =
  let
    fun getCharAtIndex(str:string, index:int, currIndex:int):char option =
      if(currIndex = size(str)) then NONE
      else if(currIndex = index) then SOME(String.sub(str, index))
      else getCharAtIndex(str, index, currIndex +1)
  in
    getCharAtIndex(str, index, 0)
  end

