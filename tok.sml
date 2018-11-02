fun main a = a;
structure Tok = 
 struct

datatype litteralType =
  String of string
  | Bool of bool
  | Number of real

datatype symbol =
    Operator of string
  | PredicateOperator of string
  | SyntaxSymbol of string

datatype Token =
    Identifier of string(*Ex: tablename, columname*)
  | Function of string (*Ex: noneof*)
  | PipeFunction of string (*Ex: upper, lower*)
  | Litteral of litteralType (*Ex: 123, "text"*)
  | Keyword of string (*Ex: from,Token filter*)
  | Symbol of symbol (*Ex: #, {*)


type TokenAtLine = Token * int;

 end;
