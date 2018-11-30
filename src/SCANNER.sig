
signature SCANNER = 
sig
  val stringSep:char
  val pfToLowerCase:string
  val pfToUpperCase:string
  val pfNumSep:string
  val pfTrim:string
  val pfCapitalized:string
  val pfBackwards:string

  val trimAndScan:(string) -> Tok.TokenAtLine list
end;
