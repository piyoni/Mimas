module Token
open FParsec
open MetaInf
open System.Numerics
type Tok =
    | TkFn                  // "->"
    | TkRel                 // "--"
    | TkSequ                // ">>"
    | TkSeQue               // "?>"
    | TkNe                  // "<>"
    | TkGtEq                // ">="
    | TkStEq                // "=<"
    | TkDotDot              // ".."
    | TkAssign              // ":="
    | TkTypeHint            // "::"

    | TkLBr                 // "("
    | TkRBr                 // ")"
    | TkLSq                 // "["
    | TkRSq                 // "]"
    | TkLCu                 // "{"
    | TkRCu                 // "}"
    | TkSemi                // ";"
    | TkQue                 // "?"

    | TkPipe                // "|"

    | TkSt                  // "<"
    | TkEq                  // "="
    | TkGt                  // ">"

    | TkAdd                 // "+"
    | TkSub                 // "-"
    | TkMul                 // "*"
    | TkDiv                 // "/"
    | TkPercent             // "%"
    | TkConc                // "`"

    | TkPow                 // "^"
    | TkNot                 // "~"

    | TkCom                 // ","
    | TkDot                 // "."

    | TkMod                 // "mod"
    | TkRem                 // "rem"
    | TkDef                 // "def"
    | TkType                // "type"
    | TkWith                // "with"
    | TkOr                  // "or"
    | TkXor                 // "xor"
    | TkAnd                 // "and"

    | TkInt    of bigint    // <int>
    | TkFloat  of double    // <float>
    | TkString of string    // <string>
    | TkIdent  of string    // <identifier>
    | TkVar    of string    // <variable>

    | TkEOF                 // end of file
    | TkError  of string    // error token 
type Token = Token of Tok*CodeSpan
let tkStr tk =
    match tk with
    | TkFn                  -> "->"
    | TkRel                 -> "--"
    | TkSequ                -> ">>"
    | TkSeQue               -> "?>"
    | TkNe                  -> "<>"
    | TkGtEq                -> ">="
    | TkStEq                -> "=<"
    | TkDotDot              -> ".."
    | TkAssign              -> ":="
    | TkTypeHint            -> "::"

    | TkLBr                 -> "("
    | TkRBr                 -> ")"
    | TkLSq                 -> "["
    | TkRSq                 -> "]"
    | TkLCu                 -> "{"
    | TkRCu                 -> "}"
    | TkSemi                -> ";"
    | TkQue                 -> "?"

    | TkPipe                -> "|"

    | TkSt                  -> "<"
    | TkEq                  -> "="
    | TkGt                  -> ">"

    | TkAdd                 -> "+"
    | TkSub                 -> "-"
    | TkMul                 -> "*"
    | TkDiv                 -> "/"
    | TkPercent             -> "%"
    | TkConc                -> "`"

    | TkPow                 -> "^"
    | TkNot                 -> "~"

    | TkCom                 -> ","
    | TkDot                 -> "."

    | TkMod                 -> "mod"
    | TkRem                 -> "rem"
    | TkDef                 -> "def"
    | TkType                -> "type"
    | TkWith                -> "with"
    | TkOr                  -> "or"
    | TkXor                 -> "xor"
    | TkAnd                 -> "and"

    | TkInt( i)             -> sprintf "int(%A)" i
    | TkFloat(f)            -> sprintf "float(%f)" f
    | TkString(s)           -> sprintf "string(%s)" s
    | TkIdent(s)            -> sprintf "identifier(%s)" s
    | TkVar(s)              -> sprintf "variable(%s)" s

    | TkEOF                 -> "EOF"
    | TkError(s)            -> sprintf "error(%s)" s



type 'a Parser = Parser<'a,unit>
let posTocs (ps:Position) (pe:Position) = {
                startcol = (int)ps.Column;
                endcol   = (int)pe.Column;
                startln  = (int)ps.Line;
                endln    = (int)pe.Line;
            }

let idEnd : unit Parser = notFollowedBy (choice [digit; asciiLetter; pchar '_'])
let tokComment: unit Parser = pstring "#" >>. skipRestOfLine true
let sep: unit Parser  = skipMany (choice [spaces1;tokComment])
let sep1: unit Parser = skipMany1 (choice [spaces1;tokComment]) <|> eof
let idsep = idEnd >>. sep1
            
let tokWithSpan p =
    let inapply (s:Position) m (e:Position) = (m, posTocs s e)
    sep>>. pipe3 getPosition p getPosition inapply .>> sep
let tokWithSpan1 p =
    let inapply (s:Position) m (e:Position) = (m, posTocs s e)
    sep>>. pipe3 getPosition p getPosition inapply .>> sep1


// parse different buildin value types
type ValType =
    | Integer   of bigint
    | Floating  of float
    | String    of string
    | Var       of string
    | Ident     of string
let tokNumber : ValType Parser =
    let rec digint (i:bigint) a =
        let digitval c =
            match c with
            | '0' -> bigint 0
            | '1' -> bigint 1
            | '2' -> bigint 2
            | '3' -> bigint 3
            | '4' -> bigint 4
            | '5' -> bigint 5
            | '6' -> bigint 6
            | '7' -> bigint 7
            | '8' -> bigint 8
            | '9' -> bigint 9
            | _   -> bigint 0
        match a with
        | [] -> i
        | c::rest -> digint (((((bigint 10)*i)+(digitval c))):bigint) rest
    let numberFormat =     NumberLiteralOptions.AllowFraction
                       ||| NumberLiteralOptions.AllowExponent
    numberLiteral numberFormat "number"
    |>> fun num ->
        if num.IsInteger then ValType.Integer (digint (bigint 0) (Seq.toList num.String))
        else ValType.Floating (float num.String)


let tokStr = ((pchar '"') >>. (many1CharsTill (anyChar) (pchar '"'))) |>> ValType.String : ValType Parser


let tokIdent : ValType Parser =
    let isAsciiIdStart c =
        isAsciiLetter c && isLower c
    let isAsciiIdContinue c =
        isAsciiLetter c || isDigit c || c = '_'
    identifier (IdentifierOptions(isAsciiIdStart    = isAsciiIdStart, isAsciiIdContinue = isAsciiIdContinue)) |>> ValType.Ident

let tokVar : ValType Parser =
    let isAsciiIdStart c =
        isUpper c || c = '_'
    let isAsciiIdContinue c =
        isAsciiLetter c || isDigit c || c = '_'
    identifier (IdentifierOptions(isAsciiIdStart    = isAsciiIdStart, isAsciiIdContinue = isAsciiIdContinue))|>> ValType.Var


let tokEOF : (unit*CodeSpan) Parser = tokWithSpan (eof |>> (fun () -> ()))
let tokDef  = tokWithSpan1 (pstring "def"  .>> idEnd)
let tokType = tokWithSpan1 (pstring "type" .>> idEnd)
let tokWith = tokWithSpan1 (pstring "with" .>> idEnd)

let tokLBR = tokWithSpan (pstring "(")
let tokRBR = tokWithSpan (pstring ")")

let tokLSQ = tokWithSpan (pstring "[")
let tokRSQ = tokWithSpan (pstring "]")

let tokLCU = tokWithSpan (pstring "{")
let tokRCU = tokWithSpan (pstring "}")

let tokCOM = tokWithSpan (pstring ",")

let tokSTEND = tokWithSpan (pstring ".")

