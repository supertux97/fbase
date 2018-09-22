(*Description*)
use "util.sml";
fun main a = print("Hello sml");

datatype litteralType =
  String of string
  | Bool of bool
  | Number of real

datatype symbol =
    Operator of string
  | PredicateOperator of string
  | SyntaxSymbol of string

datatype Token =
    Identifier of string (*Ex: tablename, columname*)
  | Function of string (*Ex: noneof*)
  | PipeFunction of string (*Ex: upper, lower*)
  | Litteral of litteralType (*Ex: 123, "text"*)
  | Keyword of string (*Ex: from,Token filter*)
  | Symbol of symbol (*Ex: #, {*);

val keywords = ["from","filter","using","and","or",
              "merge","insert","rows","into","remove","as",
              "where", "set","create","table","with",
              "colums","of","default","string","boolean","number",
              "output"]
val functions = ["upper","lower","oneof","noneof"]
val pipeFunctions = ["upper","lower"]
val operators = ["+","-","*","/"]
val predicateOperators = ["=","!=","<",">","<=",">="]
val syntaxSymbols = [".",",",":","{","}","(", ")","*","|"]

fun rmWs(str:string):string =
  case (getCharAtIndex(str,0), getCharAtIndex(str,1))  of
      (SOME(#" "), SOME(#" ")) => rmWs(String.substring(str,1, size(str) -1))
    | (SOME(_), SOME(_)) => String.substring(str,0,1) ^ rmWs(String.substring(str,1,size(str) -1))
    | (_, _) => str (*The first or both are empty*)

fun rmComments(lines: string list):string list =
  case lines of
    (x::xs) =>
    let
      val substr = Substring.full(x)
      val (noComment, comment) = Substring.splitl (fn c => c <> #"#") substr
    in
      Substring.string noComment :: rmComments(xs)
    end
    |(nil) => lines

fun splitStrByNewline(str:string):string list =
  let val substr = Substring.full(str)
  in
    map (fn e => Substring.string e) (Substring.fields (fn c => c = #"\n") substr )
  end

fun getTokenByKind(t:string):Token =
  let val tComparator = member(t)
  in
    if tComparator keywords then Keyword(t)
    else if tComparator functions then Function(t)
    else if tComparator pipeFunctions then PipeFunction(t)
    else if tComparator operators then Symbol(Operator(t))
    else if tComparator predicateOperators then Symbol(PredicateOperator(t))
    else if tComparator syntaxSymbols then Symbol(SyntaxSymbol(t))
    else Identifier(t)
  end

(* fun trimAndScan(str:string):Token list =
  let
    val noComments = rmComments(splitStrByNewline(str))
    val noCommentOrWhitespace = rmWs(listToStr(noComments,I," "))
  in
    scan(str, String.sub(0))
  end *)

(* val _ = print(trimAndScan("dette    er    ern test\nnylinje   mel\n #kommentarHeleLinja\n linjestart #kommentarslutt")) *)
(* fun scan(str:string, currChar:char):Token list = *)
