use "util.sml";
fun main a = a

(*A tiny function for performing unit-testing.*)
fun test(desc:string, expected:''a, result:''a, toStr:(''a->string)) =
  if result = expected then print(Util.formatln("[PASSED]: $", [desc]))
  else print(Util.formatln("[FAILED]: $ Expected: $ but got $",[desc, toStr(expected),
  toStr(result)]))

(*
fun testExpectsException(funcToTest,ex:exn) = 
  (funcToTest();false)
  handle ex => true

fun matchTest(i:int ) = 
  case i of 
     1 => 1  *)
