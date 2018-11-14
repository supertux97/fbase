use "util/listUtil.sml";
use "operators.sml";
fun main a = a
structure Util = 
struct
exception NotEnoughListElements of string
exception ConvesionFormatException of string
exception ConversionException of string

(*
=======================
======MISC=============
=======================
 *)
fun fileToStr(fname:string):string =
  TextIO.inputAll(TextIO.openIn(fname))

fun println(msg:string) = print(msg ^ "\n")
fun op $ (n:int) = Int.toString(n)

fun I(a) = a


(*
=======================
======STRINGS AND CHARS
=======================
 *)
fun ssToStr(s:substring) = Substring.string s
fun rmHeadOfString(str:string):string = String.substring(str,1,size(str)-1)
fun rmTailOfString(str:string):string = String.substring(str,0,size(str)-1)
fun strToSs(s:string) = Substring.full s
fun tlString(str:string):string = String.substring(str,1,size(str) -1)
fun tlStringOpt(str:string):string option = 
  if size(str) = 0 then NONE 
  else SOME(String.substring(str,1,size(str) -1))
fun hdString(str:string):char = String.sub(str,0)
fun hdStringOpt(str:string):char option = 
  if size(str) = 0 then NONE
  else SOME(String.sub(str,0))

fun chrToStr(c) = Char.toString(c)

(*Splits a string into parts defined by a delimter. If the delimiter appears at
 the back, an empty string is returned as the last elem*)
fun splitStr(str:string, delim:char):string list = 
  if String.size(str) = 0 then []
  else 
    let val (firstPart,rest) = Substring.splitl (fn c=>c<>delim) (strToSs(str))
    in ssToStr(firstPart) :: (if Substring.isEmpty(rest) then []
                                      else 
                                        if ssToStr(rest) = Char.toString(delim)
                                        then [""] 
                                        else splitStr(tlString(ssToStr(rest)),delim))
  end

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

(*Example usage: format("Name: $1 Age: $2", ["Name", $15]*)
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
  end

fun formatln(str:string, vals:string list) = 
  format(str,vals) ^ "\n"

fun repeatStr(str:string,numTimes:int):string = 
    if numTimes <= 0 then str 
    else str ^ repeatStr(str,numTimes -1)

fun getCharAtIndex(str:string, index:int):char option =
  let
    fun getCharAtIndex(str:string, index:int, currIndex:int):char option =
      if(currIndex = size(str)) then NONE
      else if(currIndex = index) then SOME(String.sub(str, index))
      else getCharAtIndex(str, index, currIndex +1)
  in
    getCharAtIndex(str, index, 0)
  end

fun splitStrByNewline(str:string):string list =
  let val substr = Substring.full(str)
  in
    map (fn e => Substring.string e) (Substring.fields (fn c => c = #"\n") substr )
  end

(*Removes exess whitespace.Two or more whitespaces are squezed into one so that
 the result is only one space. In addition single spaces at the start or end is
 removed*)

fun rmMultipleSpace(str:string):string = 
  case (getCharAtIndex(str,0), getCharAtIndex(str,1))  of
      (SOME(#" "), SOME(#" ")) => rmMultipleSpace(String.substring(str,1, size(str) -1))
    | (SOME(_), SOME(_)) => String.substring(str,0,1) ^ rmMultipleSpace(String.substring(str,1,size(str) -1))
    | (_, _) => str (*The first or both are empty*)

fun rmSpaceAtStartOrEnd(str:string):string =
  let val noMultipleSpace = rmMultipleSpace(str)
  in 
    case (getCharAtIndex(noMultipleSpace,0),
    getCharAtIndex(noMultipleSpace,size(noMultipleSpace) -1)) of
         (SOME(#" "),SOME(#" ")) => noMultipleSpace |> rmHeadOfString |> rmTailOfString 
        |(SOME(#" "),_) => tlString(noMultipleSpace)
        |(SOME(_),SOME(#" ")) => rmTailOfString(noMultipleSpace)
        |other => noMultipleSpace
  end

(*Removes the firsst occurence of a character in a string. If the character is
not found, the original string is returned untouched*)
fun rmFirstCharMatchOfString(c:char,str:string) = 
    let val (beforeMatch, afterMatch) = Substring.splitl (fn curr => curr <> c) (strToSs(str))
    in ssToStr beforeMatch ^ rmHeadOfString(ssToStr(afterMatch))
  end

(*
=======================
======NUMBERS=============
=======================
 *)

(*A version which uses the Real-libarys function for parsing, but resturns the
real or raises an exeption instead of returning a option*)
fun realFromString(str:string):real =
 case Real.fromString(str) of
     SOME(r) => r
   | NONE => raise ConvesionFormatException(format("Could not parse the string intoa real", [str]))

end;
