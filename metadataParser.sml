use "map/map.sml";
use "map/map.sig";
use "ErrorHandler.sml";
use "scanner.sml";
use "METADATA_PARSER.sig";
fun main a = a

structure MetadataParser = 
struct 
datatype Type = STRING | NUMBER | BOOL 
(*For each field*)
datatype FieldInfo = fieldInfoNoDefault of string * Type |
                    fieldInfoDefault of string * Type * litteral
val fieldDelim = #";"
val idString = #"s"
val idNumber = #"n"
val idBool = #"b"
val endOfInfo = #"}"

fun chrToStr(c) = Char.toString(c)

fun getFieldName(finfo:FieldInfo):string = 
  case finfo of
       fieldInfoDefault(name,_,_) => name
      |fieldInfoNoDefault(name,_) => name

fun getFieldType(finfo:FieldInfo):Type = 
  case finfo of
       fieldInfoDefault(_,type_,_) => type_
      |fieldInfoNoDefault(_,type_) => type_

fun isSameType(type1:Type,type2:Type) = 
  case (type1,type2) of
    (STRING, STRING) => true
    |(NUMBER,NUMBER) => true
    |(BOOL,BOOL) => true
    |(_,_) => false

fun typeToStr(t:Type):string = 
      case t of 
           STRING => "string"
          |NUMBER => "number"
          |BOOL => "bool"

(*Gets the associatted default value and ensures that it is of the requested
  type. An exception is trhown if not*)
fun getDefaultVal(fd:FieldInfo):litteral = 
  case fd of
        fieldInfoDefault(_,_,default) => (
          case default of
              (*Default strings is padded to look like a regular string*)
                Tok.String(s) =>
                 let val strSep = Char.toString(Scanner.stringSep)
                 in
                  Tok.String(
                    Util.format("$$$",[strSep, TokUtil.litteralToStr(default),
                    strSep]))
                 end
                (*Ugly workaround to prevent a wierd type error caused by multiple
                 import*)
                |other => default)
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
           | other => raise ErrorHandler.unexpectedSymbol("number, string or boolean", other ^ " in metadatafile", 1)
      end
  end

fun ofSameType(t:Type, lit:Tok.litteral) = 
  case (t,lit) of
       (STRING, Tok.String(_)) => true
      |(NUMBER, Tok.Number(_)) => true
      |(BOOL, Tok.Bool(_)) => true
      |(_,_) => false

(*Splits a single field of metadata information into the corresponding parts.
 A pair of the fieldname and the information is returned*)
fun parseSingleField(source:string,filename:string):FieldInfo = 
  let val (name,rest) = ParseUtil.getFirstIdentifier(source) 
      val info = Util.splitStr(Util.tlString(rest), #",")
      in  case info of 
          [typeStr,defValStr] => 
            let val type_ = getTypeByIdentifier(typeStr) 
                val defVal = getLitteral(defValStr)
            in
              if ofSameType(type_, defVal) then 
                fieldInfoDefault(name, type_, defVal)
              else 
               raise ErrorHandler.typeErrorDefaultVal(typeToStr(type_),
               TokUtil.getLitteralKind(defVal), name, filename)
           end
         |[type_] => 
             fieldInfoNoDefault(name, getTypeByIdentifier(type_))
         |other => raise ErrorHandler.malformedMetadata("type OR type,default value", ListUtil.listToStr(other,Util.I,"")) 
  handle Subscript => raise ErrorHandler.malformedMetadata(source,"syntax in acordance to the manual")
  end


fun fieldInfoToStr(fi:FieldInfo,idx:int) = 
    case fi of 
         fieldInfoDefault(name, t, defVal) => Util.format("Name:$ Idx:$ type_:$ DefVal: $", 
             [name, Util.$(idx), typeToStr(t),TokUtil.litteralToStr(defVal)])
        |fieldInfoNoDefault(name, t) => Util.format("Name: $ Idx:$ type_:$ ", 
             [name, Util.$(idx), typeToStr(t)])

  (*Extracts information from a string of metadata information into a list of
   tupples consisting of the fieldname and the associated information*)
  fun parseFileInfoFromFieldList (fields:string list,filename:string):(FieldInfo) list = 
    let fun inner(fieldsIn, fieldsOut) = 
      case fieldsIn of 
        (x::xs) => 
          let val field = parseSingleField(x,filename) 
          in inner(xs, field :: fieldsOut)
         end
        |[] => fieldsOut
    in
      inner(fields, [])
    end

(*Parses position and type information from a metadata source. Type information
includes type of the field and possibly a default value *)
fun parse(source:string,filename:string): FieldInfo list = 
  let val fieldMap = StrMap.empty() 
      val fields = Util.splitStr(source,fieldDelim) 
      val fieldInfo = parseFileInfoFromFieldList(fields,filename) 
  in 
   rev(fieldInfo)
  end

end;
