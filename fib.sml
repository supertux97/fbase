use "util.sml";
fun main a = a

fun fib 0 = 1
  | fib 1 = 1
  | fib n = fib(n-1) + fib(n-2)

fun fibUpto(0): string = $(fib(0))
  | fibUpto(remaining) = $(fib(remaining)) ^ ", " ^  fibUpto(remaining -1);

val _ = print(fibUpto(10))
