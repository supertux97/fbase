signature MAP = 
sig
  type 'a MapTree
  type keyType
  val insert: ('a MapTree*keyType*'a)-> 'a MapTree
  val get: ('a MapTree*keyType) -> 'a option
  val empty:(unit) -> 'a MapTree
end;
