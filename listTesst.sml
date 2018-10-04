use "util.sml";
fun main a = a

fun listElem(l:int list) =
   case l of 
        [x] => 1
      |  [] => 0
      | _ => 99

val _ = print($(listElem([1])))
