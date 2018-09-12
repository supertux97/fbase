(*Description*)
fun main a = print("Hello sml");

datatype Literal =
   Bool of bool
  |Number of int
  |String of string

datatype Keyword =
   Alfabetic of string
  |Character of char

datatype Token =
    Name of string
  | Function of string
  | Litteral
  | Keyword;

val keywords = ["from","as","merge"]
val functions = ["upper","lower"];

fun getCharAtIndex(str:string, index:int):char option =
  let
    fun getCharAtIndex(str:string, index:int, currIndex:int):char option =
      if(currIndex = size(str)) then NONE
      else if(currIndex = index) then SOME(String.sub(str, index))
      else getCharAtIndex(str, index, currIndex +1)
  in
    getCharAtIndex(str, index, 0)
  end

fun getOptionChar(c:char option) =
case c of
   SOME(c) => Char.toString(c)
  |NONE => "Not found";


(* fun rmWs(str:string):string =
  let
    fun rmWs (str:string, SOME(c), NONE):string = str
    | rmWs(str:string, SOME(#" "), SOME(#" ")):string =
      let
        val _ = print("at41: Str: [" ^ str ^ "] c1: [" ^ "space" ^ "] c2: [" ^ "space" ^ "] " ^ "\n")
      in
      rmWs(String.substring(str,1, size(str) -2 ), getCharAtIndex(str, 2), getCharAtIndex(str,3))
      end
    | rmWs(str, NONE, NONE) = str
    | rmWs(str, _, SOME(#" ")) = rmWs()
    |rmWs(str:string, c1:char option, c2:char option) =
    let
      val _ = print("at 48 Str: [" ^ str ^ "] c1: [" ^ getOptionChar(c1) ^ "] c2: [" ^ getOptionChar(c2) ^ "]c1Pos: " ^ "\n")
    in
      String.substring(str, 0, 2)
      ^ rmWs(String.substring(str, 2, size(str) - 2),  getCharAtIndex(str, 2), getCharAtIndex(str, 3) )
    end
in
  rmWs(str, getCharAtIndex(str,0), getCharAtIndex(str,1))
end *)

fun rmWs(str:string) =
  let
    fun rmWs(str:string, SOME(#" "), SOME(#" ")):string = rmWs(String.substring(str,2, size(str) -2), getCharAtIndex(str,1), getCharAtIndex(str,2))
      | rmWs(str:string, SOME(_), SOME(_)):string = String.substring(str,0,1) ^ rmWs(String.substring(str,1,size(str) -1), getCharAtIndex(str,1), getCharAtIndex(str,2))
      | rmWs(str:string, _, _) = str
  in
    rmWs(str, getCharAtIndex(str,0), getCharAtIndex(str,1))
  end

val removed = rmWs("ab   bcd")
val _ = print(removed)

(* fun scan(str:string, currChar:char):Token list =

  val trimmed =
    if currChar == #"#" then "test"
    else str; *)
