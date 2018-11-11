fun main a = a;

fun insert(a,b):{test:int,test2:int} = 
  {test=a,test2=b}

val a = insert(1,2)
val _ = print(Int.toString(#tes2 a))
