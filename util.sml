fun main s = print("main")

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

fun op $ (n:int) = Int.toString(n);

fun insertionSort [] = []
  | insertionSort (l:int list) =
    let
      val (minElem, minIdx) = minOfIntList(l)
      val _ = print("elem: " ^ $minElem ^ " index: " ^ $minIdx ^ "\n")
    in minElem :: insertionSort(removeElemAtIndex(l,minIdx))
    end

fun listToStr ([], toStr:('a->string),sep:string):string = ""
  | listToStr(x::xs, toStr:('a->string),sep:string) = toStr x ^ sep ^ listToStr(xs,toStr,sep)

exception NotEnoughListElements of string;

(*Usage: printf("Name: $1 Age: $2", [S("Name"), I(15)])*)
fun format(str:string, vals:string list) =
  let
    val chars = String.explode(str)
    val (hdChar,tlChar) = (hd(chars),tl(chars))
  in
    case (hdChar, tlChar) of
       (#"$", nil) => if vals = nil then raise NotEnoughListElements("Missing list elements")
                      else String.substring(str,1,size(str) -1) ^ hd vals
      |(_,nil) => str
      |(#"$",r) =>
        if(vals = nil) then raise NotEnoughListElements("Missing list elements")
        else hd vals ^ format(listToStr(r,Char.toString,""), tl vals)
      |(f,r) => Char.toString(f) ^ format(listToStr(r,Char.toString,""), vals)
  end
  val _ = print(format("Navn: $ Etternavn: $", ["hei","sann"]))
  (* val _ = print("Hello sml"); *)
