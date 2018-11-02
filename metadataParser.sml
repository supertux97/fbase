use "util.sml";
use "map/map.sml";
use "ErrorHandler.sml";
use "util/listUtil.sml";
fun main a = a
structure MetadataParser = 
struct 
datatype TYPE = STRING | INT | BOOL 
datatype defaultVal = i of int | s of string | b of bool
type posAndTypeSingle = int * TYPE * defaultVal
type posAndTypeNested = posAndTypeSingle list 
datatype posAndType = posAndType | posAndTypeNested

val fieldDelim = #";"
fun parseSingleField(str:string) = 
  let val (name,rest) = Substring.splitl(Char.isAlpha) (Util.strToSs(str))
      val info = Util.splitStr(Util.tlString(Util.ssToStr(rest)), #",")
  in 
    case info of 
         [kind,defVal] =>  
        |[kind] => 
        |other => raise ErrorHandler.malformedMetadata("type OR type,default value", ListUtil.toString(other,Util.I,"")) 
  handle Subscript => raise ErrorHandler.malformedMetadata(str,"syntax in acordance to the manual")

(*Parses position and type information from a metadata source.
 A map of the following format is returned: Map{fieldName:posAndType, fieldName2:posAndType,...} *)
fun getFieldInfo(source:string) = 
  let val parts = Util.splitStr(source,fieldDelim) 
      val _ = List.map parseSingleField parts
  in 5
  end

  val metadata = "name{s,\"jon doe\"};salary{i};adress(name{s}no{i});isPartTime{b}"
  val fields = getFieldInfo(metadata)
  
end;
