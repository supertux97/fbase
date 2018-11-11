use "util/listUtil.sml";
use "util/util.sml";
use "tok.sml";

fun main a = a
structure TokUtil = 
struct
type TokenAtLine = Tok.TokenAtLine

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
fun tokToStr(t:Tok.Token,toStrFunc:(string*string->string)):string =
     case t of
     Tok.Identifier i => toStrFunc(i, "Id")
    |Tok.Function f => toStrFunc(f, "Fun")
    |Tok.PipeFunction pf => toStrFunc(pf, "PipeFun")
    |Tok.Keyword k => toStrFunc(k,"Keyword")
    |Tok.Litteral l =>
      ( case l of
         Tok.String s => toStrFunc(s, "String")
        |Tok.Bool b => toStrFunc( if b then "true" else "false", "Bool")
        |Tok.Number n => toStrFunc(Real.toString(n), "Num") )
    |Tok.Symbol s =>
          (case s of
            Tok.Operator opp => toStrFunc(opp, "Op")
            |Tok.PredicateOperator po => toStrFunc(po, "PredOp")
            |Tok.SyntaxSymbol ss => toStrFunc(ss, "Syntax"))

fun litteralToStr(lit:Tok.litteral) =
  case lit of
      Tok.Number(n) => Real.toString(n)
     |Tok.String(s) => s
     |Tok.Bool(b) => if b then "true" else "false"

fun valOfTok(t:TokenAtLine) = 
  tokToStr(getTok(t),tokVal)

fun tokAtLineToTok(tal:TokenAtLine):Tok.Token = 
  case tal of (tok,_) => tok

fun listOfTokenAtLineToToks(tals:Tok.TokenAtLine list):Tok.Token list = 
  List.map tokAtLineToTok tals

fun litteralEquals(l1:Tok.litteral,l2:Tok.litteral) = 
  case (l1,l2) of
    (Tok.Number(n1), Tok.Number(n2) ) => 
      Real.toString(n1) = Real.toString(n2)   
    |(Tok.Bool(b1), Tok.Bool(b2)) => b1 = b2
    |(Tok.String(s1), Tok.String(s2)) => s1 = s2 
    |other => false
end;
