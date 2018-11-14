use "util/listUtil.sml";
use "util/util.sml";
use "tok.sml";

fun main a = a
structure TokUtil = 
struct

(*Gets the first tree tokens and then the rest*)
fun getFirstTripple(toks:Tok.TokenAtLine
  list):(Tok.TokenAtLine*Tok.TokenAtLine*Tok.TokenAtLine*Tok.TokenAtLine list) =
   ( List.nth(toks,0), List.nth(toks,1), List.nth(toks,2), ListUtil.dropN(toks, 2))  

fun tokValAndKind(value:string, kind:string) = Util.format("$:$", [value, kind])
fun tokVal(value:string, kind:string) = value
fun tokKind(value:string, kind:string) = kind

fun getTok(tal: Tok.TokenAtLine) = 
 case tal of (t,_) => t 

fun getLineNo(tal:Tok.TokenAtLine) =
  case tal of(_,lineNo) => lineNo
  
fun mkTokAtLine(tok:Tok.Token, line:int):Tok.TokenAtLine = (tok, line)

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

fun valOfTok(t:Tok.TokenAtLine) = 
  tokToStr(getTok(t),tokVal)

fun tokAtLineToTok(tal:Tok.TokenAtLine):Tok.Token = 
  case tal of (tok,_) => tok

fun getTokKind(tok:Tok.Token):string = 
  tokToStr(tok, tokKind)

fun listOfTokenAtLineToToks(tals:Tok.TokenAtLine list):Tok.Token list = 
  List.map tokAtLineToTok tals

fun litteralEquals(l1:Tok.litteral,l2:Tok.litteral) = 
  case (l1,l2) of
    (Tok.Number(n1), Tok.Number(n2) ) => 
      Real.toString(n1) = Real.toString(n2)   
    |(Tok.Bool(b1), Tok.Bool(b2)) => b1 = b2
    |(Tok.String(s1), Tok.String(s2)) => s1 = s2 
    |other => false

fun getLitteralKind(l:Tok.litteral) = 
  case l of
    Tok.String(_) => "string"
   |Tok.Number(_) => "integer"
   |Tok.Bool(_) => "boolean"

fun tokAtLineListToStr(tl:Tok.TokenAtLine list):string =
  case tl of
     [] => ""
    |(x::xs) =>
      case x of
        (tok, lineNo) =>
          Util.format("[$] $", [Util.$lineNo, tokToStr(tok,tokValAndKind)]) ^
          "\n" ^ tokAtLineListToStr(xs)
end;
