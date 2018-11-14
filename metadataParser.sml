use "map/map.sml";
use "map/map.sig";
use "ErrorHandler.sml";
use "scanner.sml";

fun main a = a

(*Provides funtions and types related to extracting, validating and organizing metadata.*)
structure MetadataParser = 
struct 
datatype Type = STRING | NUMBER | BOOL 

(*Metadata for each field*)
datatype fieldInfo = fieldInfoNoDefault of string * Type | (*Field name*Type of field*)
                    fieldInfoDefault of string * Type * litteral (*Field name*Type of field*default value*)

val fieldDelim = #";"
val endOfInfo = #"}"

(*Used to signify the field type*)
val idString = #"s"
val idNumber = #"n"
val idBool = #"b"


fun getFieldName(finfo:fieldInfo) = 
  case finfo of
       fieldInfoDefault(name,_,_) => name
      |fieldInfoNoDefault(name,_) => name

fun getFieldType(finfo:fieldInfo) = 
  case finfo of
       fieldInfoDefault(_,type_,_) => type_
      |fieldInfoNoDefault(_,type_) => type_

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


(*Extracts the default value and applies formatting*)
fun getDefaultVal(fd:fieldInfo) = 
  case fd of
        fieldInfoDefault(_,_,default) => (case default of
            (*Default strings is padded to look like a regular string*)
             Tok.String(s) =>
               let val strSep = Char.toString(Scanner.stringSep)
               in
                Tok.String(
                  Util.format("$$$",[strSep, TokUtil.litteralToStr(default),
                  strSep]))
               end
            |other => default )
       |unonown => raise Match


(*Gets the type by reading the first character of the string*)
fun getTypeByIdentifier(source:string): Type = 
  let val firstChar = Util.hdString(source)
  in
    if firstChar = idString then STRING
    else if firstChar = idNumber then NUMBER
    else if firstChar = idBool then BOOL 
    else raise ErrorHandler.unexpectedSymbol(Util.format("$, $ or $",
        [Util.chrToStr(idString), Util.chrToStr(idNumber),
         Util.chrToStr(idBool)]), Util.chrToStr(firstChar) ^" in metadata file",1)
  end


(*Gets the first litteral from a string. The string is expected
 to be correctly formated*)
fun getLitteral(source:string):litteral= 
  let val firstChar = Util.hdString(source)
  in
    if ParseUtil.isStartOfString(firstChar) then 
        String(ParseUtil.strFromBeginningOfStr(source, Scanner.stringSep))
    else if ParseUtil.isStartOfDigit(firstChar) then
      let val (firstNum,rest) = ParseUtil.getFirstNumberFromString(source)
      in Number(firstNum)
      end

    else  
      let val (id,rest) = Substring.splitl (fn c=>c <> endOfInfo) (Util.strToSs(source))
      in case Util.ssToStr(id) of 
            "true" => Bool(true)
           |"false" => Bool(false)
           |other => raise ErrorHandler.unexpectedSymbol("number, string or boolean", other ^ " in metadatafile", 1)
      end
  end


(*Splits a single field of metadata information into the corresponding parts
defined in fieldInfo
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


(*Extracts information from a list of metadata information  into a list of
 tupples consisting of the fieldname and the associated metadata for the field*)
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
   rev(fieldInfo)
  end

end;
