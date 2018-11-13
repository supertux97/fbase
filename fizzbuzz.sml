fun main a = a

fun fizzbuzz(0) = ""
   |fizzbuzz(n) = 
  let val curr = case (n mod 3 = 0, n mod 5 = 0) of
    (true,true) => "fizzbuzz"
    |(true,_) => "fizz"
    |(_,true) => "buzz"
    |(_,_) => Int.toString(n)
  in curr ^ "\n" ^ fizzbuzz(n-1)
  end

val _ = print(fizzbuzz(20))
