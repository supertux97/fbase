fun main a = a

infix 3 |> 
fun (x:'a) |> (f:('a->'b)) = f x
