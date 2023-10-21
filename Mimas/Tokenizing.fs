module Tokenizing
open Token
open System.Numerics
open FParsec
open MetaInf
open PAST

let first x _ = x
type 'a Parser = Parser<'a,unit>
let posTocs (ps:Position) (pe:Position) = {
                startcol = (int)ps.Column;
                endcol   = (int)pe.Column;
                startln  = (int)ps.Line;
                endln    = (int)pe.Line;
            }
let constrASTNode ps node pe = ASTNode(node,(),posTocs ps pe)

let tokComment: unit Parser = pstring "#" >>. skipRestOfLine true
let sep: unit Parser  = skipMany (choice [spaces1;tokComment])
let sep1: unit Parser = skipMany1 (choice [spaces1;tokComment]) <|> eof

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
let tokWithSpan1 p =
    let inapply (s:Position) m (e:Position) =
        Token( m,
            {
                startcol = (int)s.Column;
                endcol   = (int)e.Column;
                startln  = (int)s.Line;
                endln    = (int)e.Line;
            }
        )
    sep>>. pipe3 getPosition p getPosition inapply .>> sep1

let tokEOF : Token Parser = tokWithSpan (eof |>> (fun () -> Tok.TkEOF))

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
let nfbc: unit Parser = notFollowedBy (anyOf "-><?.:=")
let tokfn       = tokWithSpan ((pstring "->"     |>> first TkFn)        .>> nfbc)
let tokrel      = tokWithSpan ((pstring "--"     |>> first TkRel)        .>> nfbc)
let toksequ     = tokWithSpan ((pstring ">>"     |>> first TkSequ)        .>> nfbc)
let tokseque    = tokWithSpan ((pstring "?>"     |>> first TkSeQue)        .>> nfbc)
let tokne       = tokWithSpan ((pstring "<>"     |>> first TkNe)        .>> nfbc)
let tokgteq     = tokWithSpan ((pstring ">="     |>> first TkGtEq)        .>> nfbc)
let toksteq     = tokWithSpan ((pstring "=<"     |>> first TkStEq)        .>> nfbc)
let tokdd       = tokWithSpan ((pstring ".."     |>> first TkDotDot)        .>> nfbc)
let tokasign    = tokWithSpan ((pstring ":="     |>> first TkAssign)        .>> nfbc)
let tokth       = tokWithSpan ((pstring "::"     |>> first TkTypeHint)        .>> nfbc)
let toklbr      = tokWithSpan  (pstring "("      |>> first TkLBr)
let tokrbr      = tokWithSpan  (pstring ")"      |>> first TkRBr)
let toklsq      = tokWithSpan  (pstring "["      |>> first TkLSq)
let tokrsq      = tokWithSpan  (pstring "]"      |>> first TkRSq)
let toklcu      = tokWithSpan  (pstring "{"      |>> first TkLCu)
let tokrcu      = tokWithSpan  (pstring "}"      |>> first TkRCu)
let toksemi     = tokWithSpan ((pstring ";"      |>> first TkSemi)        .>> nfbc)
let tokque      = tokWithSpan ((pstring "?"      |>> first TkQue)        .>> nfbc)
let tokpipe     = tokWithSpan ((pstring "|"      |>> first TkPipe)        .>> nfbc)
let tokst       = tokWithSpan ((pstring "<"      |>> first TkSt)        .>> nfbc)
let tokeq       = tokWithSpan ((pstring "="      |>> first TkEq)        .>> nfbc)
let tokgt       = tokWithSpan ((pstring ">"      |>> first TkGt)        .>> nfbc)
let tokadd      = tokWithSpan ((pstring "+"      |>> first TkAdd)        .>> nfbc)
let toksub      = tokWithSpan ((pstring "-"      |>> first TkSub)        .>> nfbc)
let tokmul      = tokWithSpan ((pstring "*"      |>> first TkMul)        .>> nfbc)
let tokconc     = tokWithSpan ((pstring "`"      |>> first TkConc)        .>> nfbc)
let tokdiv      = tokWithSpan ((pstring "/"      |>> first TkDiv)        .>> nfbc)
let tokpercent  = tokWithSpan ((pstring "%"      |>> first TkPercent)        .>> nfbc)
let tokpow      = tokWithSpan ((pstring "^"      |>> first TkPow)        .>> nfbc)
let toknot      = tokWithSpan ((pstring "~"      |>> first TkNot)        .>> nfbc)
let tokcom      = tokWithSpan ((pstring ","      |>> first TkCom)        .>> nfbc)
let tokdot      = tokWithSpan ((pstring "."      |>> first TkDot)        .>> nfbc)
let tokmod      = tokWithSpan ((pstring "mod"    |>> first TkMod)        .>> nfbc)
let tokrem      = tokWithSpan ((pstring "rem"    |>> first TkRem)        .>> nfbc)
let tokdef      = tokWithSpan ((pstring "def"    |>> first TkDef)        .>> nfbc)
let toktype     = tokWithSpan ((pstring "type"   |>> first TkType)        .>> nfbc)
let tokwith     = tokWithSpan ((pstring "with"   |>> first TkWith)        .>> nfbc)
let tokor       = tokWithSpan ((pstring "or"     |>> first TkOr)        .>> nfbc)
let tokxor      = tokWithSpan ((pstring "xor"    |>> first TkXor)        .>> nfbc)
let tokand      = tokWithSpan ((pstring "and"    |>> first TkAnd)        .>> nfbc)


let mappingInfix  f str _ (ASTNode(l,(),csl)) (ASTNode(r,(),csr)) =
    ASTNode(f str (ASTNode(l,(),csl)) (ASTNode(r,(),csr)),(),codeSpanMerge csl csr)
let mappingPrefix f str _ (ASTNode(e,(),cs)) =
    ASTNode(f str (ASTNode(e,(),cs)),(),cs)

let addInfixOperator (pars:OperatorPrecedenceParser<_,_,_>) str prec assoc f =
    let op = InfixOperator(str, getPosition .>> sep, prec, assoc, (), mappingInfix f str)
    pars.AddOperator(op)
let addPrefixOperator (pars:OperatorPrecedenceParser<_,_,_>) str prec assoc f =
    let op = PrefixOperator(str, getPosition .>> sep, prec, assoc, (), mappingPrefix f str )
    pars.AddOperator(op)

let addOps0 p f =
    addInfixOperator p ";"  1 Associativity.Right f
    addInfixOperator p "?"  1 Associativity.Right f
    addInfixOperator p "?>" 1 Associativity.Right f
let addOps1 p f =
    addInfixOperator p "|"  2 Associativity.Right f
let addOps2 p f =
    addInfixOperator p "xor"  3 Associativity.Left f
    addInfixOperator p "or"   3 Associativity.Left f
let addOps3 p f =
    addInfixOperator p "and"  4 Associativity.Left f
let addOps4 p f =
    addInfixOperator p "=<"  5 Associativity.Left f
    addInfixOperator p ">="  5 Associativity.Left f
    addInfixOperator p "<>"  5 Associativity.Left f
    addInfixOperator p "<"   5 Associativity.Left f
    addInfixOperator p "="   5 Associativity.Left f
    addInfixOperator p ">"   5 Associativity.Left f
let addOps5 p f =
    addInfixOperator p "+"  6 Associativity.Left f
    addInfixOperator p "-"  6 Associativity.Left f
    addInfixOperator p "`"  6 Associativity.Left f
let addOps6 p f =
    addInfixOperator p "*"   7 Associativity.Left f
    addInfixOperator p "/"   7 Associativity.Left f
    addInfixOperator p "%"   7 Associativity.Left f
    addInfixOperator p "mod" 7 Associativity.Left f
    addInfixOperator p "rem" 7 Associativity.Left f
let addOps7 p f =
    addInfixOperator p "^"  8 Associativity.Left f
let addOpsPre p f = 
    addPrefixOperator p "-" 9 true f
    addPrefixOperator p "+" 9 true f
    addPrefixOperator p "~" 9 true f





// bracketed things
let parseThunk p f = between toklcu tokrcu p |>> (fun (ASTNode(x,(),cs)) -> ASTNode(f (ASTNode(x,(),cs)), (), cs))
let parseBr    p = between toklbr tokrbr p

let parseSeq p f = 
    let args = sepBy p (tokcom)
    pipe3 getPosition (between toklsq tokrsq args) getPosition (fun ps ls pe -> ASTNode(f ls,(),posTocs ps pe))

let parseArgList p =
    let args = sepBy p (tokcom)
    opt (between toklbr tokrbr args)
//
let parseIdentOrFn p i f = 
    (tokWithSpan tokIdent .>>. parseArgList p) |>> fun (Token(t,cs),oargs) ->
        match t with
        | TkIdent s ->
            match oargs with
            | None -> ASTNode(i s,(),cs)
            | Some(args) -> ASTNode(f(s,args),(),cs)
        | _ -> raise (invalidArg "i" "")

let parseVar f =
    tokWithSpan tokVar |>> fun (Token(t,cs)) ->
        match t with
        | TkVar s -> ASTNode(f s,(),cs)
        | _ -> raise (invalidArg "i" "")
let parseNumber i f =
    tokWithSpan tokNumber |>> fun (Token(t,cs)) ->
        match t with
        | TkInt   s -> ASTNode(i s,(),cs)
        | TkFloat s -> ASTNode(f s,(),cs)
        | _ -> raise (invalidArg "i" "")

let parseString f =
    tokWithSpan tokNumber |>> fun (Token(t,cs)) ->
        match t with
        | TkString   s -> ASTNode(f s,(),cs)
        | _ -> raise (invalidArg "i" "")

// type parsing
let typeInfixop str arg0 arg1 = ASTType.LitFn(str,[arg0; arg1])
let typePrefixop str arg = ASTType.LitFn(str,[arg])

let parsetypehint p pt f =
    let fast (ASTNode(h,(),csh)) (ASTNode(t,(),cst)) = ASTNode(f(ASTNode(h,(),csh),ASTNode(t,(),cst)),(),codeSpanMerge csh cst)
    let th = opt (pstring "::" >>. pt)
    pipe2 p th (fun h ot ->
    match ot with
    | None     -> h
    | Some(tp) -> fast h tp
    )

let typeOpsParser = OperatorPrecedenceParser()
addOps0 typeOpsParser typeInfixop
addOps1 typeOpsParser typeInfixop

let parseBaseType p = 
    let terms = choice [
        parseIdentOrFn p ASTType.LitId ASTType.LitFn;
        parseSeq   p ASTType.LitSeq;
        parseBr    p;
        parseThunk p ASTType.Thunk
        parseVar ASTType.Var
        //parsetypehint p typeOpsParser ASTType.GiveType
    ]
    parsetypehint terms typeOpsParser ASTType.GiveType
typeOpsParser.TermParser <- parseBaseType typeOpsParser 



// pattern parsing
let patternInfixop str arg0 arg1 = ASTPattern.LitFn(str,[arg0; arg1])
let patternPrefixop str arg = ASTPattern.LitFn(str,[arg])
let patternOpsParser = OperatorPrecedenceParser()
addOps0   patternOpsParser patternInfixop
addOps1   patternOpsParser patternInfixop
addOps2   patternOpsParser patternInfixop
addOps3   patternOpsParser patternInfixop
addOps4   patternOpsParser patternInfixop
addOps5   patternOpsParser patternInfixop
addOps6   patternOpsParser patternInfixop
addOps7   patternOpsParser patternInfixop
addOpsPre patternOpsParser patternPrefixop



let parseBasePattern p = 
    let terms = choice [
        parseIdentOrFn p ASTPattern.LitId ASTPattern.LitFn;
        parseSeq   p ASTPattern.LitSeq;
        parseBr    p;
        parseThunk p ASTPattern.Thunk
        parseVar ASTPattern.Var
        //parsetypehint p typeOpsParser ASTPattern.GiveType
    ]
    parsetypehint terms typeOpsParser ASTPattern.GiveType
patternOpsParser.TermParser <- parseBasePattern patternOpsParser
// parse expressions
let exprInfixop str arg0 arg1 = ASTExpr.LitFn(str,[arg0; arg1])
let exprPrefixop str arg = ASTExpr.LitFn(str,[arg])
let exprOpsParser = OperatorPrecedenceParser()
addOps0   exprOpsParser exprInfixop
addOps1   exprOpsParser exprInfixop
addOps2   exprOpsParser exprInfixop
addOps3   exprOpsParser exprInfixop
addOps4   exprOpsParser exprInfixop
addOps5   exprOpsParser exprInfixop
addOps6   exprOpsParser exprInfixop
addOps7   exprOpsParser exprInfixop
addOpsPre exprOpsParser exprPrefixop

let parseBaseExpr p = 
    let terms = choice [
        parseIdentOrFn p ASTExpr.LitId ASTExpr.LitFn;
        parseSeq   p ASTExpr.LitSeq;
        parseBr    p;
        parseThunk p ASTExpr.Thunk
        parseVar ASTExpr.Var
        parseNumber ASTExpr.LitInt ASTExpr.LitFloat
        parseString ASTExpr.LitString
    ]
    parsetypehint terms typeOpsParser ASTExpr.GiveType
exprOpsParser.TermParser <- parseBaseExpr exprOpsParser


//type ASTStatemenType<'b> = 
//    | LitDef        of string * (ASTNode<'b ASTType,'b> list   ) Option* ASTNode<'b ASTType,'b>
//    | LitTypeDef    of string * (ASTNode<'b ASTType,'b> list   ) Option
//    | LitRule       of string * (ASTNode<'b ASTPattern,'b> list) Option* ASTNode<'b ASTExpr,'b>
//    | LitRuleType   of string * (ASTNode<'b ASTType,'b> list   ) Option* ASTNode<'b ASTType,'b>
let parseLitDef =
    let consASTLitDef ident args _ ret =
        match ident with
        | TkIdent s -> ASTStatemenType.LitDef(s,args,ret)
        | _ -> raise (invalidArg "i" "")
    let parseInternal = (tokdef >>. pipe4 tokIdent (parseArgList typeOpsParser) tokfn typeOpsParser consASTLitDef)
    pipe3 getPosition parseInternal getPosition constrASTNode

let parseLitTypeDef =
    let consASTLitTypeDef ident args =
        match ident with
        | TkIdent s -> ASTStatemenType.LitTypeDef(s,args)
        | _ -> raise (invalidArg "i" "")
    let parseInternal = (toktype >>. pipe2 tokIdent (parseArgList typeOpsParser) consASTLitTypeDef)
    pipe3 getPosition parseInternal getPosition constrASTNode

let parseLitRule =
    let consASTLitRule head _ ret = ASTStatemenType.LitRule(head,ret)
    let parseInternal = pipe3 (parseIdentOrFn exprOpsParser ASTExpr.LitId ASTExpr.LitFn) tokfn exprOpsParser consASTLitRule
    (pipe3 getPosition parseInternal getPosition constrASTNode)

let parseLitTypeRule =
    let consASTLitTypeRule head _ ret = ASTStatemenType.LitRuleType(head,ret)
    let parseInternal = pipe3 typeOpsParser tokrel typeOpsParser consASTLitTypeRule
    (pipe3 getPosition parseInternal getPosition constrASTNode)


let parseStatementType = choice [parseLitDef; parseLitTypeDef;attempt parseLitTypeRule; parseLitRule]

let parseStatement, parseStatementRef = createParserForwardedToRef()

let rec parseStatementAST =
    let pst (ASTNode(st,(),cs)) x =
        match x with
        | None    -> ASTNode(ASTStatement(st,[]),(),cs)
        | Some(s) -> ASTNode(ASTStatement(st,s),(),cs)
    (pipe2 parseStatementType (opt (tokwith >>. toklsq >>. (many parseStatement) .>> tokrsq)) pst) .>> tokdot


let parseType str = 
    match run (typeOpsParser .>> eof) str with
    | Success(result, _, _)   -> sprintf "%A" result
    | Failure(errorMsg, _, _) -> errorMsg
let parseExpr str = 
    match run exprOpsParser str with
    | Success(result, _, _)   -> sprintf "%A" result
    | Failure(errorMsg, _, _) -> errorMsg

let parseStatements str = 
    match run ((many parseStatementAST) .>> eof) str with
    | Success(result, _, _)   -> sprintf "%A" result
    | Failure(errorMsg, _, _) -> errorMsg

let parseFile str = 
    match runParserOnFile ((many parseStatementAST) .>> eof) () str System.Text.Encoding.ASCII with
    | Success(result, _, _)   -> sprintf "%A" result
    | Failure(errorMsg, _, _) -> errorMsg