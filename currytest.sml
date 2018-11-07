use "listUtil.sml";
fun main a = a

fun times x n = x * n
val l = [1,2,3,4,5]

val l2 = List.map (times 2) l
val _ = print(ListUtil.listToStr(l2, Int.toString, " "))

