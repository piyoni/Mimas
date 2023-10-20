module Token

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