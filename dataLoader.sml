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
datatype Type = STRING | NUMBER | BOOL 
datatype litteral = num of real| str of string | boolean of bool

(*For each field*)
datatype fieldInfo = fieldInfoNoDefault of int * Type |
                    fieldInfoDefault of int * Type * litteral
type  singleField = string * fieldInfo

val fieldDelim = #";"
val idString = #"s"
val idNumber = #"n"
val idBool = #"b"

fun chrToStr(c) = Char.toString(c)


(*Gets the type by reading the first character of the string*)
fun getTypeByIdentifier(source:string): Type = 
  let val firstChar = Util.hdString(source)
  in
    if firstChar = idString then STRING
    else if firstChar = idNumber then NUMBER
    else if firstChar = idBool then BOOL 
    else raise ErrorHandler.unexpectedSymbol(Util.format("$, $ or $",
        [chrToStr(idString), chrToStr(idNumber), chrToStr(idBool)]), chrToStr(firstChar) ^" in metadata file",1)
  end


(*Gets the first litteral from a string. The string is expected
 to be correctly formated*)
fun getLitteral(source:string):litteral = 
  let val firstChar = Util.hdString(source)
  in
    if ParseUtil.isStartOfString(firstChar) then 
      str(Util.takeWhileStr(Util.tlString(source),(fn c=>c <> Scanner.stringSep)) )

    else if ParseUtil.isStartOfDigit(firstChar) then
      let val (firstNum,rest) = ParseUtil.getFirstNumberFromString(source)
      in num(firstNum)
      end

    else  
      let val (id,rest) = Substring.splitl (fn c=>c <> Scanner.stringSep) (Util.strToSs(source))
      in case Util.ssToStr(id) of 
           "true" => boolean(true)
           |"false" => boolean(false)
           | other => raise ErrorHandler.unexpectedSymbol("number, string or boolean", other ^ " in metadatafile", 1)
      end
  end

(*Splits a single field of metadata information into the corresponding parts.
 A pair of the fieldname and the information is returned*)
fun parseSingleField(source:string, fieldIndex:int):singleField= 
  let val (name,rest) = ParseUtil.getFirstIdentifier(source) 
      val info = Util.splitStr(Util.tlString(rest), #",")
      in  case info of 
          [type_,defVal] => 
            (name, fieldInfoDefault(fieldIndex, getTypeByIdentifier(type_), getLitteral(defVal)))
         |[type_] => 
             (name, fieldInfoNoDefault(fieldIndex, getTypeByIdentifier(type_)))
         |other => raise ErrorHandler.malformedMetadata("type OR type,default value", ListUtil.listToStr(other,Util.I,"")) 
  handle Subscript => raise ErrorHandler.malformedMetadata(source,"syntax in acordance to the manual")
  end

fun fieldInfoToStr(name:string, fi:fieldInfo) = 
    let fun typeToStr(t:Type) = 
          case t of 
               STRING => "string"
              |NUMBER => "number"
              |BOOL => "bool"
        fun getDefVal(d:litteral) = 
          case d of
              num(n) => Real.toString(n)
             |str(s) => s
             |boolean(b) => if b then "true" else "false"
    in
    case fi of 
         fieldInfoDefault(idx, t, defVal) => Util.format("Name:$ Idx:$ type_:$ DefVal: $", 
             [name, Util.$(idx), typeToStr(t),getDefVal(defVal)])
        |fieldInfoNoDefault(idx,t) => Util.format("Name: $ Idx:$ type_:$ ", 
             [name, Util.$(idx), typeToStr(t)])
  end

  (*Extracts information from a string of metadata information into a map where
   the key of the map is the name of the field and the value is the associated
   information*)
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

(*Parses position and type_ information from a metadata source. Type information
includes type of the field and possibly a default value
 A map of the following format is returned: Map{fieldName:singleField, fieldName2:singleField,...} *)
fun getFieldInfo(source:string) = 
  let val fieldMap = StrMap.empty() 
      val fields = Util.splitStr(source,fieldDelim) 
      val mapped = mapFieldInfo(fields) 
  in 
    mapped
  end

  val metadata = "name{s,'jon doe'};salary{n};adress{s};isPartTime{b}"
  val fields = getFieldInfo(metadata)
  
end;
