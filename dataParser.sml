use "map/map.sml";
use "tok.sml";
structure DataParser = 
struct
fun main a = a

(*Uses a metadatamap to parse a list of data-rows into a list of maps. Each map 
 contains a mapping between the column-name and the actual value. If no value
 appears in the field, but a default value is defined in the metadata for this
 field, that value is used. Othervise, an exception is trown.
 The function is tail-recursive*)
fun parse(metadataMap: fieldInfo StrMap.Map): (string StrMap.Map) list = 
  let  fun parseInner(metadataMap,rows) =
  in 
    parseInner(metadataMap,[])
  end
end;
