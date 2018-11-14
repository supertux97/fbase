use "util/tokUtil.sml";
use "ErrorHandler.sml";

(*Funtintions applied to Tok.litteral. The functions are to be used in 
 a pipeline in the query.*)
structure PipeFunctions =
struct

fun raiseTypeError(found:Tok.litteral, required:string) = 
  raise ErrorHandler.pipeFunctionTypeError(
            TokUtil.litteralToStr(found),required)

fun transformStr(t:Tok.litteral, eachCharFunc: (char -> char)) = 
 case t of 
  Tok.String(s) => 
    let val chars = String.explode(s) 
          val upperCaseChars = List.map eachCharFunc chars
      in  
       Tok.String(String.implode(upperCaseChars))
      end 
  |other => raiseTypeError(other,"string")

fun toUpper(t:Tok.litteral) = 
  transformStr(t, Char.toUpper)

fun toLower(t:Tok.litteral) = 
 transformStr(t, Char.toLower) 


(*Creates a string consisting of the number with a space for each third digit
 Ex: 555000 -> 1 000*)
fun numSep(n: Tok.litteral) = 
  let fun performSep(c::chars, currDigit):char list = 
           (case currDigit mod 3 of
               0 => c :: #" " :: performSep(chars, currDigit + 1) 
              |n => c :: performSep(chars,currDigit + 1))

         |performSep([], currDigit) = []
    in 
      case n of 
          Tok.Number(n) => 
            let
              val numbers = Real.toString(n)
              val numbersChars = String.explode(numbers)
              val revNumbers = rev(numbersChars)

              val decimalPart = #"." :: ListUtil.takeWhile(revNumbers,(fn c => c <> #".")) 
              val numberPart = tl(ListUtil.dropWhile(revNumbers,(fn c => c <> #".")))
              val numSeperatedRev = performSep(numberPart,1)
              val numSeperated = rev(numSeperatedRev)
          in
            Tok.String(String.implode(numSeperated @ decimalPart))
          end
         |other => raiseTypeError(other, "number")
   end

(*Makes the first character upper-case*)
fun capitalized(s:Tok.litteral) = 
  case s of
    Tok.String(s) => 
        let val upper = s |> Util.hdString |> Char.toUpper |> Char.toString
            val rest = String.substring(s, 1, size(s) -1)
        in Tok.String(upper ^ rest)
        end
   |other => raiseTypeError(other,"string") 

end;
