fun main a = a

(*Functions for working with lists. The functions will work lists of any type*)
structure ListUtil =
struct 

(*The identity function*)
fun I(e:'a) =  e

(*Checks if a element is included in a list
adapted with variations from "ML for the working programmer"*)
fun member(x) l = List.exists (fn elem => elem = x) l

(*Fills a new list with elements of the passed list as long as a predicate for
the element is true*)
fun takeWhile(l:'a list, pred:('a->bool)) = 
  case l of 
      [] => [] 
     |(x::xs) => if pred(x) then x::takeWhile(xs,pred) 
                 else []

(*Fills a new list with elements of the passed list unitil the predicate for
the element is true*)
fun takeWhileNot(l:'a list, pred:('a->bool)) = 
  takeWhile(l,(fn e=>not(pred(e))))

fun isNonEmpty(l:'a list) = List.length(l) > 0

(*Creates a copy of the passed list. As long as the predicate is true, elements
 from the passed list is not included into the returned*)
fun dropWhile(l:'a list, pred:('a->bool)) = 
  case l of 
       [] => []
     |(x::xs) => if pred(x) then dropWhile(xs, pred)
                 else l

(*Creates a copy of the passed list. As long as the predicate is not true, elements
 from the passed list is not included into the returned*)
fun dropWhileNot(l:'a list, pred:('a->bool)) = 
  dropWhile(l,(fn e=>not(pred(e))))

(*Returns the list with element at index i removed. If is out of bounds, no
 element is removed*)
fun removeElemAtIndex(l:'a list,i:int) =
let
  fun removeElemAtIndex([], i:int, curr:int) = []
    | removeElemAtIndex(x::xs, i, curr) =
      if curr = i then xs
      else x:: removeElemAtIndex(xs, i, curr+1)
  in
    removeElemAtIndex(l,i,0)
  end


(*Creates a string representation of the given list. toStr is used for
converting the elemtns to a string and sep is used in between the elements*)
fun listToStr ([], toStr:('a->string), sep:string):string = ""
  | listToStr([x], toStr, sep) = toStr x
  | listToStr(x::xs, toStr, sep) = toStr x ^ sep ^ listToStr(xs,toStr,sep)

(*Creates as string repsetentation of a given twodimentional list. toStr is used
 for converting the elemtns to a string. sepElem is used between the elements
  and sepBetweenLists is used between the sublists*)
fun twoDimListToStr(twoDim:'a list list,toStr:('a->string), sepElem:string,sepBetweenLists:string) = 
  let val listOfStrings =  List.map (fn l => listToStr(l,toStr, sepElem)) twoDim
      val oneString = listToStr(listOfStrings, I, sepBetweenLists)
  in oneString
  end

(*Finds the largest elemetn of the list.
 A supplied comparator is used for the comparision. Compare returns whether the
 first element is bigger than the second*)
fun max(l:'a list, compare:'a*'a->bool):'a = 
  let fun inner(l,biggest) =  
    case l of 
      (x::xs) => if compare(x,biggest) then 
                    inner(xs,x)
                else inner(xs,biggest)
      |[] => biggest
  in 
    inner(l,List.nth(l,0))
  end

fun intGreatherThan(a:int, b:int) = a > b

(*A variant of the map function where the callback is a tuple constisting of the
 element and the index*)
fun mapWithIndex mapFn l = 
  let fun doMap(mapFn,l,idx) = 
      case l of 
        (x::xs) => mapFn(x,idx) :: doMap(mapFn,xs,idx + 1)
       |[] => []
  in 
    doMap(mapFn,l,0)
  end

(*Splits a list into a list of lists with a predicate function. Every time the
predicate is true, the elements gets added into a new list. The element for wich
the predicate is true is not added to the sublist
 Example: ["test1","test2","!","test3","!"] -> [ ["test1","test2"],["test3"] ]*)
fun splitListIntoSublists(l:'a list, pred:('a->bool)):'a list list = 
       if List.length(l) > 0 then
            takeWhileNot(l,pred):: 
              let val restAfterFirstSublist = 
                  let val restIncludingDelim = dropWhileNot(l,pred)
                  in if List.length(restIncludingDelim) <= 1 then [] else tl(restIncludingDelim) end
              in splitListIntoSublists(restAfterFirstSublist, pred)
              end
       else []
end;
