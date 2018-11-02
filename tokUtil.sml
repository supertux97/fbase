use "listUtil.sml";
use "util.sml";
use "tok.sml";
open Tok

fun main a = a
structure TokUtil = 
struct
type TokenAtLine = Token * int;
(*Gets the first tree tokens and then the rest*)
fun getFirstTripple(toks:TokenAtLine list):(TokenAtLine*TokenAtLine*TokenAtLine*TokenAtLine list) =
   ( List.nth(toks,0), List.nth(toks,1), List.nth(toks,2), ListUtil.dropN(toks, 2))  

fun tokValAndKind(value:string, kind:string) = Util.format("$:$", [value, kind])
fun tokVal(value:string, kind:string) = value
fun tokKind(value:string, kind:string) = kind

fun getTok(tal: TokenAtLine) = 
 case tal of (t,_) => t 

fun getLineNo(tal:TokenAtLine) =
  case tal of(_,lineNo) => lineNo
  
(*Creates a string representation of a given token. The supplied function
toStrFunc allows for various kinds of formatting*)
fun tokToStr(t:Token,toStrFunc:(string*string->string)):string =
     case t of
     Identifier i => toStrFunc(i, "Id")
    |Function f => toStrFunc(f, "Fun")
    |PipeFunction pf => toStrFunc(pf, "PipeFun")
    |Keyword k => toStrFunc(k,"Keyword")
    |Litteral l =>
      ( case l of
         String s => toStrFunc(s, "String")
        |Bool b => toStrFunc( if b then "true" else "false", "Bool")
        |Number n => toStrFunc(Real.toString(n), "Num") )
    |Symbol s =>
          (case s of
             Operator opp => toStrFunc(opp, "Op")
            |PredicateOperator po => toStrFunc(po, "PredOp")
            |SyntaxSymbol ss => toStrFunc(ss, "Syntax"))

  fun valOfTok(t:TokenAtLine) = tokToStr(getTok(t), tokVal)
end;
