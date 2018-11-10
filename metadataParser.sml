use "util/util.sml";
use "map/map.sml";
use "map/map.sig";
use "util/parseUtil.sml";
use "ErrorHandler.sml";
use "scanner.sml";

fun main a = a

structure MetadataParser = 
struct 
datatype Type = STRING | NUMBER | BOOL 

(*For each field*)
datatype fieldInfo = fieldInfoNoDefault of string * Type |
                    fieldInfoDefault of string * Type * Tok.litteral
val fieldDelim = #";"
val idString = #"s"
val idNumber = #"n"
val idBool = #"b"
val endOfInfo = #"}"

fun chrToStr(c) = Char.toString(c)

fun getFieldName(finfo:fieldInfo) = 
  case finfo of
       fieldInfoDefault(name,_,_) => name
      |fieldInfoNoDefault(name,_) => name

fun getFieldType(finfo:fieldInfo) = 
  case finfo of
       fieldInfoDefault(_,type_,_) => type_
      |fieldInfoNoDefault(_,type_) => type_

fun getDefaultVal(fd:fieldInfo) = 
  case fd of 
        fieldInfoDefault(_,_,default) => default
       |unonown => raise Match
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
fun getLitteral(source:string):Tok.litteral= 
  let val firstChar = Util.hdString(source)
  in
    if ParseUtil.isStartOfString(firstChar) then 
        Tok.String(ParseUtil.strFromBeginningOfStr(source, Scanner.stringSep))
    else if ParseUtil.isStartOfDigit(firstChar) then
      let val (firstNum,rest) = ParseUtil.getFirstNumberFromString(source)
      in Tok.Number(firstNum)
      end

    else  
      let val (id,rest) = Substring.splitl (fn c=>c <> endOfInfo) (Util.strToSs(source))
      in case Util.ssToStr(id) of 
           "true" => Tok.Bool(true)
           |"false" => Tok.Bool(false)
           | other => raise ErrorHandler.unexpectedSymbol("number, string or boolean", other ^ " in metadatafile", 1)
      end
  end

(*Splits a single field of metadata information into the corresponding parts.
 A pair of the fieldname and the information is returned*)
fun parseSingleField(source:string):fieldInfo = 
  let val (name,rest) = ParseUtil.getFirstIdentifier(source) 
      val info = Util.splitStr(Util.tlString(rest), #",")
      in  case info of 
          [type_,defVal] => 
            fieldInfoDefault(name, getTypeByIdentifier(type_), getLitteral(defVal))
         |[type_] => 
             fieldInfoNoDefault(name, getTypeByIdentifier(type_))
         |other => raise ErrorHandler.malformedMetadata("type OR type,default value", ListUtil.listToStr(other,Util.I,"")) 
  handle Subscript => raise ErrorHandler.malformedMetadata(source,"syntax in acordance to the manual")
  end

fun typeToStr(t:Type) = 
      case t of 
           STRING => "string"
          |NUMBER => "number"
          |BOOL => "bool"

fun fieldInfoToStr(fi:fieldInfo,idx:int) = 
    case fi of 
         fieldInfoDefault(name, t, defVal) => Util.format("Name:$ Idx:$ type_:$ DefVal: $", 
             [name, Util.$(idx), typeToStr(t),TokUtil.litteralToStr(defVal)])
        |fieldInfoNoDefault(name, t) => Util.format("Name: $ Idx:$ type_:$ ", 
             [name, Util.$(idx), typeToStr(t)])

  (*Extracts information from a string of metadata information into a list of
   tupples consisting of the fieldname and the associated information*)
  fun parseFileInfoFromFieldList (fields:string list):(fieldInfo) list = 
    let fun inner(fieldsIn, fieldsOut) = 
      case fieldsIn of 
        (x::xs) => 
          let val field = parseSingleField(x) 
          in inner(xs, field :: fieldsOut)
         end
        |[] => fieldsOut
    in
      inner(fields, [])
    end

(*Parses position and type information from a metadata source. Type information
includes type of the field and possibly a default value
 A list of the following format is returned: Map{fieldName:singleField, fieldName2:singleField,...} *)
fun parse(source:string): fieldInfo list = 
  let val fieldMap = StrMap.empty() 
      val fields = Util.splitStr(source,fieldDelim) 
      val fieldInfo = parseFileInfoFromFieldList(fields) 
  in 
   fieldInfo 
  end

(*TESTING*)
val shouldPrint = false
val metadata = "name{s,'jon doe'};salary{n};adress{s};isPartTime{b}"
val fields = parse(metadata)
val fieldsStr = ListUtil.mapWithIndex fieldInfoToStr fields
val _ = print(if shouldPrint then ListUtil.listToStr(fieldsStr, Util.I, "\n") else "")
end;
