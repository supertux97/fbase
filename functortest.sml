fun main a = a

signature ADD = 
sig
  val add:(int*int) -> int
end;

structure add1 :> ADD =
struct
  fun add(a:int,b:int) = a + b
end;

