use "util/util.sml";
use "ordered.sig";
use "map/map.sig";
use "ordered.sml";
use "tok.sml";
fun main a = a

(*An fully functional inplementation of a map(aka associative array or dictonary
for those that speak snake-thounge) which uses a tree for storage and
retrival. Insert and get is in principle O(log(n)) but can slow down to O(n) in
 case the tree gets balanced. This implementation does not handle balancing.*)
functor MapTreeOfType (ordered:ORDERED_TYPE):MAP = 
struct 
  type keyType = ordered.t

  datatype 'a Map = 
    (*Key*value*left*right*)
    Node of keyType * 'a * 'a Map * 'a Map |
    EmptyNode

  type 'a Map = 'a Map
  (*Creates a empty map. The map does not yet have a value-type associated. This is set upon the first insert*)
  fun empty() = EmptyNode

  (*Inserts a value into the given map. If the key already exists, no value is
  inserted*)
  fun insert(mt:'a Map, key:keyType,value: 'a) = 
    case mt of 
      EmptyNode => Node(key,value,EmptyNode, EmptyNode) 
     |Node(k,v,l,r) => case ordered.compare(k,key) of
                         EQUAL => mt
                        |LESS => Node(k,v,insert(l,key,value),r)
                        |GREATER => Node(k,v,l,insert(r,key,value))
  (*Retrieves the value associated with the given key. The result is wrapped in
   an option*)
  fun get(mt: 'a Map, key:keyType) = 
    case mt of 
      EmptyNode => NONE 
     |Node(k,v,l,r) => case ordered.compare(k,key) of 
                         EQUAL => SOME(v)
                        |LESS => get(l,key) 
                        |GREATER => get(r,key)

  (*Adds the entries of two maps togheter to form a single map. The maps
  have to be of the same kind.*)
  fun addTwoMaps(m1:'a Map, m2:'a Map):'a Map = 
    let fun addToMap(from,to) = 
      case from  of 
       Node(k,v,l,r) => let val this = insert(to,k,v)
                            val left = addToMap(l,this)
                            val right = addToMap(r,left)
                        in right
                        end
      |EmptyNode => to
    in addToMap(m1,m2)
    end

end;

