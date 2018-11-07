use "util/util.sml";
use "map/map.sml";
use "util/parseUtil.sml";
use "ErrorHandler.sml";
use "util/listUtil.sml";
use "map/map.sml";
use "map/map.sig";
use "scanner.sml";

fun main a = a
structure MetadataParser = 
struct 
datatype TYPE = STRING | NUMBER | BOOL 
datatype litteral = n of real| s of string | b of bool

(*For each field*)
datatype fieldInfo = fieldInfoNoDefault of int * TYPE |
                    fieldInfoDefault of int * TYPE * litteral
type singleField = string * fieldInfo
val fieldDelim = #";"
val idString = #"s"
val idNumber = #"n"
val idBool = #"b"

fun chrToStr(c) = Char.toString(c)
fun getKindById(source:string):TYPE = 
  let val c = Util.hdString(source)
  in
    if c = idString then STRING
    else if c = idNumber then NUMBER
    else if c = idBool then BOOL 
    else raise ErrorHandler.unexpectedSymbol(Util.format("$, $ or $",
        [chrToStr(idString), chrToStr(idNumber), chrToStr(idBool)]), chrToStr(c) ^" in metadata file",1)
  end

(*Gets the corresponding of litteral from a string. The string is expected
 to be correctly formated*)
fun getLitteral(source:string):litteral = 
  let val firstChar = Util.hdString(source)
  in
    if ParseUtil.isStartOfString(firstChar) then 
      s(Util.takeWhileStr(Util.tlString(source),(fn c=>c <> Scanner.stringSep)) )

    else if ParseUtil.isStartOfDigit(firstChar) then
      let val (num,rest) = ParseUtil.getFirstNumberFromString(source)
      in n(num)
      end

    else  
      let val (id,rest) = Substring.splitl (fn c=>c <> Scanner.stringSep) (Util.strToSs(source))
      in case Util.ssToStr(id) of 
           "true" => b(true)
           |"false" => b(false)
           | other => raise ErrorHandler.unexpectedSymbol("true OR false", other ^ " in metadatafile", 1)
      end
  end

(*Splits a single field of metadata information into the corresponding parts.
 A pair of the fieldname and the information is returned*)
fun parseSingleField(str:string, fieldIndex:int):singleField= 
  let val (name,rest) = ParseUtil.getFirstIdentifier(str) 
      val info = Util.splitStr(Util.tlString(rest), #",")
      in  case info of 
          [kind,defVal] => 
            (name, fieldInfoDefault(fieldIndex, getKindById(kind), getLitteral(defVal)))
         |[kind] => 
             (name, fieldInfoNoDefault(fieldIndex, getKindById(kind)))
         |other => raise ErrorHandler.malformedMetadata("type OR type,default value", ListUtil.listToStr(other,Util.I,"")) 
  handle Subscript => raise ErrorHandler.malformedMetadata(str,"syntax in acordance to the manual")
  end

fun fieldInfoToStr(name:string, fi:fieldInfo) = 
    let fun typeToStr(t:TYPE) = 
          case t of 
               STRING => "string"
              |NUMBER => "number"
              |BOOL => "bool"
        fun getDefVal(d:litteral) = 
          case d of
              n(num) => Real.toString(num)
             |s(str) => str
             |b(bo) => if bo then "true" else "false"
    in
    case fi of 
         fieldInfoDefault(idx, t, defVal) => Util.format("Name:$ Idx:$ Type:$ DefVal: $", 
             [name, Util.$(idx), typeToStr(t),getDefVal(defVal)])
        |fieldInfoNoDefault(idx,t) => Util.format("Name: $ Idx:$ Type:$ ", 
             [name, Util.$(idx), typeToStr(t)])
  end

  fun mapFieldInfo(fields:string list) = 
    let fun mapInner(map, fields, index) = 
      case fields of 
       (x::xs) => 
          let val field = parseSingleField(x,index) 
          in
            case field of 
              (name, fieldInfo) => mapInner(StrMap.insert(map, name, fieldInfo), xs, index +1)
         end
        |[] => map
    in
      mapInner(StrMap.empty(), fields,0)
    end

(*Parses position and type information from a metadata source.
 A map of the following format is returned: Map{fieldName:posAndType, fieldName2:posAndType,...} *)
fun getFieldInfo(source:string) = 
  let val fieldMap = StrMap.empty() 
      val fields = Util.splitStr(source,fieldDelim) 
      val mapped = mapFieldInfo(fields) 
    val _ = print( (
          case StrMap.get(mapped, "name") of SOME(e) =>
            fieldInfoToStr("name",e)))
      
  in 5
  end

  val metadata = "name{s,'jon doe'};salary{n};adress{s};isPartTime{b}"
  val fields = getFieldInfo(metadata)
  
end;
