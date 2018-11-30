use "map/mapTree.sml";
use "util/listUtil.sml";
structure IntMap = MapTreeOfType(orderedInt)
structure StrMap = MapTreeOfType(orderedString)

val m = StrMap.empty()
val m1 = StrMap.insert(m,"a",1)
val m2 = StrMap.insert(m1,"b",2)
val m3 = StrMap.insert(m2,"c",3)
