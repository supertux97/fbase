signature METADATA_PARSER = 
sig
  type litteral
  datatype Type = STRING | NUMBER | BOOL
  datatype FieldInfo = fieldInfoNoDefault of string * Type |
                    fieldInfoDefault of string * Type * litteral

  val typeToStr:(Type) -> string
  val getFieldType:(FieldInfo) -> Type
  val getFieldName:(FieldInfo) -> string
  val getDefaultVal:(FieldInfo) -> litteral
end;
