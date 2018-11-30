fun main a = a
structure ListUtil =
struct 

(*Checks if a element is included in a list
adapted with variations from "ML for the working programmer"*)
fun member(x) l = List.exists (fn elem => elem = x) l

(*Adopted from "Ml for the working programmer"*)
infix memberOf
fun (x memberOf []) = false
| (x memberOf (e::r)) = (x=e) orelse (x memberOf r)

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

(*The identity function*)
fun I(e:'a) =  e

fun replaceElemAtIndex(l:'a list,i:int,new:'a) = 
  let val listBefore = List.take(l,i)
      val listAfter = List.drop(tl(l),i) 
  in 
    rev(new ::listBefore) @  listAfter
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


(*Creates a list filled with the numbers n,n-1,...,1.
 fillList(5) will therefore give a 5 element long list*)
fun fillList(n) = 
  let fun fillInner(0,l) = l
        | fillInner(n,l) = fillInner(n-1, n::l)
  in
    fillInner(n,[])
  end

fun fillWith(elem:'a,num:int):'a list = 
  case num of 
   0 => []
  |n => elem :: fillWith(elem,num-1)

(*Compare retunrs wheter the first element is bigger than the second*)
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

fun dropN([], n:int) = []
  | dropN(l,0) = l
  | dropN(x::xs,n) = dropN(xs,n-1) 

(*The sum of all elements in a list of integers or reals*)
fun sum [] = 0
   |sum(x::xs) = x + sum(xs)

fun sumTail(l) = 
  let fun sumFrom([], summed) = summed
	   |sumFrom(x::xs,summed) = sumFrom(xs,summed + x)
  in
	  sumFrom(l, 0)
  end

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
