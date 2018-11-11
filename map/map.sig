signature MAP = 
sig
  type 'a Map 
  type keyType
  val insert: ('a Map*keyType*'a)-> 'a Map 
  val get: ('a Map*keyType) -> 'a option
  val empty:(unit) -> 'a Map
  val addTwoMaps:('a Map*'a Map)->'a Map
end;
