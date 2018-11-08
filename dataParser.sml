use "map/map.sml";
use "tok.sml";
use "metadataParser.sml";
open MetadataParser
structure DataParser = 
struct
val fieldDelim = #";"
val stringSep = #"'"
fun main a = a
(*Tries to get the first litteral from a string source*)
fun getLitteralFromType(
      source:string, type_:MetadataParser.Type, lineNo:int,
      filename:string):Tok.litteral option = 

  let fun raiseWrongDataExn(expected, found) = 
        raise ErrorHandler.typeErrorStoredData(
                expected, MetadataParser.typeToStr(found),lineNo,filename)

      val firstCharOpt = Util.hdStringOpt(source)
  in
      case firstCharOpt of
           SOME(firstChar) => 
              (case type_ of
               STRING => if ParseUtil.isStartOfString(firstChar) then
                          let val firstStr = ParseUtil.strFromBeginningOfStr(source,stringSep)
                              val strPadded = ParseUtil.padStartAndEndStr(firstStr,stringSep)
                              val tokStr = Tok.String(strPadded)
                          in
                            SOME(tokStr)
                          end
                         else 
                            raiseWrongDataExn("string", type_)
              |NUMBER => if ParseUtil.isStartOfDigit(firstChar) then
                            SOME(Tok.Number(ParseUtil.getFirstNumberFromStringAsLitteral(source)))
                         else
                           raiseWrongDataExn("integer", type_)
              |BOOL  => if ParseUtil.isStrBoolean(source) then
                            SOME(Tok.Bool(ParseUtil.strToBool(source)))
                        else 
                          raiseWrongDataExn("boolean", type_) )
          |NONE => NONE 
  end
fun parseSingleFieldIntoMap(
      field:string, metadata:MetadataParser.fieldInfo, map:Tok.litteral StrMap.Map, lineNo:int,
      filename:string) = 

      let val fieldType = MetadataParser.getFieldType(metadata) 
          val fieldName = MetadataParser.getFieldName(metadata)
          val fieldValueOption = getLitteralFromType(field,fieldType,lineNo,filename)
      in case metadata of 
         fieldInfoNoDefault(_,_)  => (case fieldValueOption of
                                      SOME(value) => StrMap.insert(map, fieldName, value)
                                     |NONE => raise ErrorHandler.missingData(
                                               filename, lineNo))
        |infoDef as fieldInfoDefault(_,_,_) => (case fieldValueOption of 
                                      SOME(value) => StrMap.insert(map, fieldName, value)
                                     |NONE =>
                                          StrMap.insert(
                                              map, fieldName,
                                              getDefaultVal(infoDef)))
      end

fun mapFields(
    fields:string list, metadata:MetadataParser.fieldInfo list,
     map:Tok.litteral StrMap.Map,lineNo:int, filename:string):Tok.litteral StrMap.Map = 
  case metadata of 
    (m::ms) => 
      (case fields of
        (f::fs) => mapFields(
                      fs,ms,
                      parseSingleFieldIntoMap(f, m, map, lineNo, filename),
                      lineNo, filename)
        |[] => map )
    |[] => map

fun parseSingleRow(metadata: MetadataParser.fieldInfo list, row:string, lineNo:int,
                   filename:string):(Tok.litteral StrMap.Map) = 

  let val fields = Util.splitStr(row,fieldDelim)
      val map = StrMap.empty()
  in  
      mapFields(fields, metadata, map,lineNo, filename)
  end

(*Uses a list of metadata information to parse a list of data-rows into a list of maps. Each map 
 represents a single row and contains a mapping between the column-name and the actual value. 
 If no value appears in the field and a default value is defined in the metadata for this
 field, that value is used. Othervise, an exception is trown.*)
fun parse(
      metadata: MetadataParser.fieldInfo list, rows:string list, 
      filename:string): (Tok.litteral StrMap.Map) list = 

  let val rowsParsed = ListUtil.mapWithIndex (fn (r,i) => parseSingleRow(metadata,r,i+1,filename)) rows
  in rowsParsed
  end

fun find(data: (Tok.litteral StrMap.Map) list, index:int, fieldName:string):string = 
  let val dataAtIndex = List.nth(data, index)
  in
    case  StrMap.get(dataAtIndex, fieldName) of 
      SOME(s) => TokUtil.litteralToStr(s)
     |NONE => "field not found"

  end 
val metadataParsed = MetadataParser.parseFieldInfo(Util.fileToStr("employees.md"))
val data = Util.fileToStr("employees.dat")  
val lines = Util.splitStr(data, #"\n")
val dataParsed = parse(rev(metadataParsed), lines, "employees.dat")

val _ = print(find(dataParsed,0,"isPartTime"))
end;
