use "util.sml";
fun main a = a

(*A tiny function for performing unit-testing.*)
fun test(desc:string, expected:''a, result:''a, toStr:(''a->string)) =
  if result = expected then print(formatln("[PASSED]: $", [desc]))
  else print(formatln("[FAILED]: $ Expected: $ but got $",[desc, toStr(expected),
  toStr(result)]))
