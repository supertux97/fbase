use "util/util.sml";
use "ordered.sig";
use "map/map.sig";
use "ordered.sml";
fun main a = a

(*An fully functional inplementation of a map(aka associative array or dictonary
for those that speak snake-thounge) which uses a tree for storage and
retrival. Insert and get is in principle O(log(n)) but can slow down to O(n) in
 case the tree gets balanced. This implementation does not handle balancing.*)
functor MapTreeOfType (ordered:ORDERED_TYPE):MAP = 
struct 
  type keyType = ordered.t

  datatype 'a MapTree = 
    Node of keyType * 'a * 'a MapTree * 'a MapTree |
    EmptyNode

  type 'a MapTree = 'a MapTree

  (*Creates a empty map. The map does not yet have a value-type associated. This is set upon the first insert*)
  fun empty() = EmptyNode

  (*Inserts a value into the given map. If the key already exists, no value is
  inserted*)
  fun insert(mt:'a MapTree, key:keyType,value: 'a) = 
    case mt of 
      EmptyNode => Node(key,value,EmptyNode, EmptyNode) 
     |Node(k,v,l,r) => case ordered.compare(k,key) of
                         EQUAL => mt
                        |LESS => Node(k,v,insert(l,key,value),r)
                        |GREATER => Node(k,v,l,insert(r,key,value))
  (*Retrieves the value associated with the given key. The result is wrapped in
   an option*)
  fun get(mt: 'a MapTree, key:keyType) = 
    case mt of 
      EmptyNode => NONE 
     |Node(k,v,l,r) => case ordered.compare(k,key) of 
                         EQUAL => SOME(v)
                        |LESS => get(l,key) 
                        |GREATER => get(r,key)
end;
