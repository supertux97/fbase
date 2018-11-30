structure orderedInt:ORDERED_TYPE = 
struct 
  type  t = int

  fun compare(i1,i2) = 
    if i1 < i2 then LESS 
    else if i1 > i2 then GREATER 
    else EQUAL
end;

structure orderedString:ORDERED_TYPE = 
struct
  type t = string
  fun compare(s1,s2) = 
    String.compare(s1,s2)
end;
