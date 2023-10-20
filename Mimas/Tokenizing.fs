module Tokenizing
open Token
open System.Numerics
open FParsec
open MetaInf



type 'a Parser = Parser<'a,unit>


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


let rec digint (i:bigint) a =
    match a with
    | [] -> i
    | c::rest -> digint (((((bigint 10)*i)+(digitval c))):bigint) rest


let numberFormat =     NumberLiteralOptions.AllowMinusSign
                   ||| NumberLiteralOptions.AllowFraction
                   ||| NumberLiteralOptions.AllowExponent

let tokNumber : Tok Parser =
    numberLiteral numberFormat "number"
    |>> fun num ->
        if num.IsInteger then Tok.TkInt (digint (bigint 0) (Seq.toList num.String))
        else Tok.TkFloat (float num.String)


let tokStr = ((pchar '"') >>. (many1CharsTill (anyChar) (pchar '"'))) |>> Tok.TkString : Tok Parser




let tokIdent : Tok Parser =
    let isAsciiIdStart c =
        isAsciiLetter c && isLower c
    let isAsciiIdContinue c =
        isAsciiLetter c || isDigit c || c = '_'
    identifier (IdentifierOptions(isAsciiIdStart    = isAsciiIdStart, isAsciiIdContinue = isAsciiIdContinue)) |>> Tok.TkIdent

let tokVar : Tok Parser =
    let isAsciiIdStart c =
        isUpper c || c = '_'
    let isAsciiIdContinue c =
        isAsciiLetter c || isDigit c || c = '_'
    identifier (IdentifierOptions(isAsciiIdStart    = isAsciiIdStart, isAsciiIdContinue = isAsciiIdContinue))|>> Tok.TkVar




let tokKW : Tok Parser =
    let matchTok t =
        match t with
        | "->"   -> TkFn                  // "->"
        | "--"   -> TkRel                 // "--"
        | ">>"   -> TkSequ                // ">>"
        | "?>"   -> TkSeQue               // "?>"
        | "<>"   -> TkNe                  // "<>"
        | ">="   -> TkGtEq                // ">="
        | "=<"   -> TkStEq                // "=<"
        | ".."   -> TkDotDot              // ".."
        | ":="   ->  TkAssign              // ":="
        | "::"   -> TkTypeHint            // "::"
        | "("    -> TkLBr                 // "("
        | ")"    -> TkRBr                 // ")"
        | "["    -> TkLSq                 // "["
        | "]"    -> TkRSq                 // "]"
        | "{"    -> TkLCu                 // "{"
        | "}"    -> TkRCu                 // "}"
        | ";"    -> TkSemi                // ";"
        | "?"    -> TkQue                 // "?"
        | "|"    -> TkPipe                // "|"
        | "<"    -> TkSt                  // "<"
        | "="    -> TkEq                  // "="
        | ">"    -> TkGt                  // ">"
        | "+"    -> TkAdd                 // "+"
        | "-"    -> TkSub                 // "-"
        | "*"    -> TkMul                 // "*"
        | "`"    -> TkConc                // "`"
        | "/"    -> TkDiv                 // "/"
        | "%"    -> TkPercent             // "%"
        | "^"    -> TkPow                 // "^"
        | "~"    -> TkNot                 // "~"
        | ","    -> TkCom                 // ","
        | "."    -> TkDot                 // "."
        | "mod"  -> TkMod                 // "mod"
        | "rem"  -> TkRem                 // "rem"
        | "def"  -> TkDef                 // "def"
        | "type" -> TkType                // "type"
        | "with" -> TkWith                // "with"
        | "or"   -> TkOr                  // "or"
        | "xor"  -> TkXor                 // "xor"
        | "and"  -> TkAnd                 // "and"
        | _      -> TkError t
    (choice [
        regex @"^(>=|=<|<>|>>|\?>|->|:=|--|\.\.|::|mod|rem|def|type|with|xor|or|and)";
        regex @"^[!@$%^&*()+\-=\[\]{};:\\|,.<>\/?`]"
        ]) |>> matchTok

let tokComment: unit Parser = pstring "#" >>. skipRestOfLine true
let sep: unit Parser = skipMany (choice [spaces1;tokComment])

let tokWithSpan p =
    let inapply (s:Position) m (e:Position) =
        Token( m,
            {
                startcol = (int)s.Column;
                endcol   = (int)e.Column;
                startln  = (int)s.Line;
                endln    = (int)e.Line;
            }
        )
    sep>>. pipe3 getPosition p getPosition inapply .>> sep

let tokTok : Token Parser = tokWithSpan( choice [
        tokKW;
        tokIdent;
        tokVar;
        tokNumber;
        tokStr
    ])
let tokEOF : Tok Parser = eof |>> (fun () -> Tok.TkEOF)
let tokenize str = 
    match run (pipe2 (many tokTok) (tokWithSpan tokEOF) (fun ls e -> ls @ [e]) ) str with
        | Success(result, _, _)   -> result
        | Failure(errorMsg, _, _) -> []

let tokenizeFile str = 
    match runParserOnFile (pipe2 (many tokTok) (tokWithSpan tokEOF) (fun ls e -> ls @ [e]) ) () str System.Text.Encoding.ASCII  with
        | Success(result, _, _)   -> result
        | Failure(errorMsg, _, _) -> []












































//open System.Text.RegularExpressions

//let matchSpaces         = Regex(@"^\s+")
//let matchComment        = Regex(@"^#.*\n")
//let matchOp1            = Regex(@"^[!@$%^&*()+\-=\[\]{};:\\|,.<>\/?]")
//let matchOp2            = Regex(@"^(>=|=<|<>|>>|\?>|->|:=|--|\.\.|::)")

//let matchFloat          = Regex(@"^[0-9]+\.[0-9]+")
//let matchInt            = Regex(@"^[0-9]+")
//let matchStr            = Regex("^\"[^\"]*\"")
//let matchWord           = Regex(@"^[a-z][a-zA-Z0-9_]*")
//let matchVar            = Regex(@"^[A-Z_][a-zA-Z0-9_]*")
//let matchErr            = Regex(@"^[^\s]+")





//let getOp2 s =
//    match s with
//        | "->" -> TkFn
//        | "--" -> TkRel
//        | ">>" -> TkSequ
//        | "?>" -> TkSeQue
//        | "<>" -> TkNe
//        | ">=" -> TkGtEq
//        | "=<" -> TkStEq
//        | ".." -> TkDotDot
//        | ":=" -> TkAssign
//        | "::" -> TkTypeHint
//        |  _   -> TkError "invalid operator"

//let getOp1 s = 
//    match s with
//        | "(" -> TkLBr                 // "("
//        | ")" -> TkRBr                 // ")"
//        | "[" -> TkLSq                 // "["
//        | "]" -> TkRSq                 // "]"
//        | "{" -> TkLCu                 // "{"
//        | "}" -> TkRCu                 // "}"
//        | ";" -> TkSemi                // ";"
//        | "?" -> TkQue                 // "?"
    
//        | "|" -> TkPipe                // "|"

//        | "<" -> TkSt                  // "<"
//        | "=" -> TkEq                  // "="
//        | ">" -> TkGt                  // ">"

//        | "+" -> TkAdd                 // "+"
//        | "-" -> TkSub                 // "-"
//        | "*" -> TkMul                 // "*"
//        | "/" -> TkDiv                 // "/"
//        | "%" -> TkPercent             // "%"

//        | "^" -> TkPow                 // "^"
//        | "~" -> TkNot                 // "~"

//        | "," -> TkCom                 // ","
//        | "." -> TkDot                 // "."
//        |  _ -> TkError "invalid operator"

//let getWord s =
//    match s with
//        | "type"   -> TkType
//        | "def"    -> TkDef
//        | "where"  -> TkDef
//        | "and"    -> TkDef
//        | "or"     -> TkDef
//        | "xor"    -> TkDef
//        | "div"    -> TkDef
//        | "mod"    -> TkDef
//        | a        -> TkIdent a

//type LexerState = {
//    Str:string;
//    Start:int
//    Tokens: Token list
//    }


//let lexerAdvance lex n f =
//    {
//        Str = lex.Str.Substring(n);
//        Start = (lex.Start+n);
//        Tokens = let tt = f (lex.Str.Substring(0,n)) in Token(tt,{start=lex.Start;ending=lex.Start+n})::lex.Tokens
//    }
//let lexerSkip lex n =
//    {
//        Str=lex.Str.Substring(n);
//        Start=lex.Start+n;
//        Tokens=lex.Tokens
//    }


//let matchToken lex = 
//    match lex.Str with
//        | a when matchSpaces.IsMatch(a)     -> let am = matchSpaces.Match(a).Length in lexerSkip lex am
//        | a when matchComment.IsMatch(a)    -> let am = matchComment.Match(a).Length in lexerSkip lex am
//        | a when matchOp2.IsMatch(a)        -> lexerAdvance lex 2 getOp2
//        | a when matchOp1.IsMatch(a)        -> lexerAdvance lex 1 getOp1
//        | a when matchFloat.IsMatch(a)      -> let am = matchFloat.Match(a).Length in lexerAdvance lex am (fun s -> TkFloat (double s))
//        | a when matchInt.IsMatch(a)        -> let am = matchInt.Match(a).Length in lexerAdvance lex am (fun s -> TkInt (BigInteger.Parse s))
//        | a when matchStr.IsMatch(a)        -> let am = matchStr.Match(a).Length in lexerAdvance lex am (fun s -> TkString (s.Substring(1,am-2)))
//        | a when matchVar.IsMatch(a)        -> let am = matchVar.Match(a).Length in lexerAdvance lex am (fun s -> TkVar s)
//        | a when matchWord.IsMatch(a)       -> let am = matchWord.Match(a).Length in lexerAdvance lex am getWord
//        | _                                 -> lexerAdvance lex (matchErr.Match(lex.Str).Length) (fun s -> TkError s)

//let rec tokenizeLineInner lex =
//    match lex.Str with
//        | "" -> lex
//        | a  -> tokenizeLineInner (matchToken lex)


//let tokenizeLine line lex =  (tokenizeLineInner {Str=line;Start=lex.Start;Tokens = lex.Tokens})
//let rec tokenizeInner lines lex =
//    match lines with
//        | [] ->  lexerAdvance lex 0 (fun s -> TkEOF)
//        | ln :: tail -> tokenizeInner tail (tokenizeLine ln lex)
//let tokenize str = (tokenizeInner str {Str="";Start=0;Tokens = []}).Tokens