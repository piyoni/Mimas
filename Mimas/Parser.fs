module Parser
    open FParsec
    open AST
    type 'a Parser = Parser<'a,unit>
    //
    let parseComment: unit Parser = pstring "#" >>. skipRestOfLine true
    //
    let sep = skipMany (choice [spaces1;parseComment]) .>> optional eof

    //
    let toCodeSpan (startpos:Position) (endpos:Position) = CodeSpan(
        {col=(int)startpos.Column;ln=(int)startpos.Line},
        {col=(int)endpos.Column;ln=(int)endpos.Line}
        )

    // parsing values
    let parseStr = ((pchar '"') >>. (many1CharsTill (anyChar) (pchar '"'))) |>> ValType.String : ValType Parser
    //
    let resultSatisfies predicate msg (p: Parser<_,_>) : Parser<_,_> =
        let error = messageError msg
        fun stream ->
            let reply = p stream
            if reply.Status <> Ok || predicate reply.Result then reply
            else Reply(Error, error)
    //
    let parseNumber : ValType Parser =
        let numberFormat =     NumberLiteralOptions.AllowFraction ||| NumberLiteralOptions.AllowExponent
        resultSatisfies (fun (num : NumberLiteral) -> num.String.[String.length num.String - 1]<>'.') "number literal cannot end with '.'" (numberLiteral numberFormat "number") |>> (fun num ->
            if num.IsInteger then ValType.Integer  num.String
            else ValType.Floating num.String)
    //
    let parseIdent : ValType Parser =
            let isAsciiIdStart c =
                isAsciiLetter c && isLower c
            let isAsciiIdContinue c =
                isAsciiLetter c || isDigit c || c = '_'
            identifier (IdentifierOptions(isAsciiIdStart    = isAsciiIdStart, isAsciiIdContinue = isAsciiIdContinue)) |>> ValType.Ident
    //
    //
    let parseVar : ValType Parser =
        let isAsciiIdStart c =
            isUpper c || c = '_'
        let isAsciiIdContinue c =
            isAsciiLetter c || isDigit c || c = '_'
        identifier (IdentifierOptions(isAsciiIdStart    = isAsciiIdStart, isAsciiIdContinue = isAsciiIdContinue))|>> ValType.Var
    //
    let parseValue : ValType Parser = (choice [parseStr;parseNumber;parseIdent;parseVar]).>>sep
    //
    //
    // parsing keywords
    let matchKW str = skipString str .>> sep
    let kwDef  = matchKW "def"
    let kwType = matchKW "type"
    let kwVar  = matchKW "var"
    let kwEnd  = matchKW "end"
    let kwIs  = matchKW "="
    let kwDot  = matchKW "."
    let kwCol  = matchKW ":"
    let kwSemi  = matchKW ";"
    let kwCom  = matchKW ","
    let kwLBr  = matchKW "("
    let kwRBr  = matchKW ")"
    let kwLSq  = matchKW "["
    let kwRSq  = matchKW "]"
    let kwLCu  = matchKW "{"
    let kwRCu  = matchKW "}"
    //
    // operator predecence parsing
    let addInfixOperator (pars:OperatorPrecedenceParser<_,_,_>) str prec assoc fn =
        let op = InfixOperator(str, getPosition.>>sep, prec, assoc, (), fn)
        pars.AddOperator(op)
    let addPrefixOperator (pars:OperatorPrecedenceParser<_,_,_>) str prec fn  =
        let op = PrefixOperator(str, getPosition.>>sep, prec, true, (), fn)
        pars.AddOperator(op)
    let exprOpsParser = OperatorPrecedenceParser()
    // infix operators
    let altFn pos l r = Expr.Alt(toCodeSpan pos pos, l, r)
    let comFn pos l r = Expr.Commma(toCodeSpan pos pos, l, r)

    let seqFn pos l r = Expr.Seq(toCodeSpan pos pos, l, r)
    let ifFn  pos l r = Expr.FnIf(toCodeSpan pos pos, l, r)
    let fnFn  pos l r = Expr.Arrow(toCodeSpan pos pos, l, r)

    let op2Fn op pos l r = Expr.Oper(toCodeSpan pos pos, op ,[l; r])
    addInfixOperator exprOpsParser "," 10 Associativity.Right comFn

    addInfixOperator exprOpsParser "|>" 20 Associativity.Right altFn
    addInfixOperator exprOpsParser "->" 30 Associativity.Right fnFn
    addInfixOperator exprOpsParser ";"  40 Associativity.Right seqFn
    addInfixOperator exprOpsParser "if" 50 Associativity.Left seqFn

    let stdinfix str prec assoc = addInfixOperator exprOpsParser str prec assoc (op2Fn (Operat str))
    stdinfix "|" 60 Associativity.Right

    stdinfix ">" 61 Associativity.Right
    stdinfix "<" 61 Associativity.Right
    stdinfix "==" 61 Associativity.Right
    stdinfix ">=" 61 Associativity.Right
    stdinfix "=<" 61 Associativity.Right
    stdinfix "<>" 61 Associativity.Right

    stdinfix "and" 62 Associativity.Left
    stdinfix "or"  62 Associativity.Left
    stdinfix "xor" 62 Associativity.Left

    stdinfix "+"   63 Associativity.Left
    stdinfix "-"   63 Associativity.Left

    stdinfix "*"   64 Associativity.Left
    stdinfix "/"   64 Associativity.Left
    stdinfix "rem" 64 Associativity.Left
    // prefix operators
    let op1Fn op pos r = Expr.Oper(toCodeSpan pos pos, op ,[r])
    let stdprefix str prec = addPrefixOperator exprOpsParser str prec (op1Fn (Operat str))
    stdprefix "+" 64 
    stdprefix "-" 64

    addPrefixOperator exprOpsParser "|>" 20 (fun p x -> x)
    // parsing declarations
    let parseTH = (opt (kwCol >>.exprOpsParser))
    let parseDeclDef = pipe5 getPosition (kwDef>>.parseIdent.>>sep) (parseTH.>>kwIs) (exprOpsParser.>>(kwEnd<?>"missing \"end\"")) getPosition (
        fun pstart idnt typehint body pend -> Decl.Def(toCodeSpan pstart pend, idnt, typehint, body)
        )
    let parseDeclVar = pipe5 getPosition (kwVar>>.sep>>.exprOpsParser) (parseTH.>>kwIs) (exprOpsParser.>>kwEnd<?>"missing\"end\"") getPosition (
        fun pstart idnt typehint body pend -> Decl.Var(toCodeSpan pstart pend, idnt, typehint, body)
        )
    let parseDecls = pipe3 getPosition (many1 (choice [parseDeclDef;parseDeclVar])) getPosition (
        fun pstart decls pend -> Expr.Decls(toCodeSpan pstart pend,decls)
        )
    // parsing list
    let rec unfoldCommas node =
        match node with
        | (Expr.Commma(_,l,r)) -> l::(unfoldCommas r)
        | _                 -> [node]
    let parseList = pipe3 (getPosition.>>kwLSq) (opt exprOpsParser) (kwRSq>>.getPosition) (
        fun pstart ex pend -> Expr.Lst(toCodeSpan pstart pend,
        match ex with
            | None     -> []
            | Some(xe) -> unfoldCommas xe
        ))
    // (expression)
    let parseExprValue = pipe3 getPosition parseValue getPosition (
        fun pstart ex pend -> Expr.Const(toCodeSpan pstart pend,ex)
        )
    let parseBrs = pipe3 (getPosition.>>kwLBr) exprOpsParser (kwRBr>>.getPosition) (
        fun pstart ex pend -> ex
        )
    let parseBaseExpr = sep>>.(choice [parseDecls;parseBrs;parseList;parseExprValue]).>> sep
    let parseApply = pipe4 getPosition parseBaseExpr (many parseBrs) getPosition (
        fun pstart head appls pend -> List.fold (fun ex a-> Expr.App(toCodeSpan pstart pend,ex,unfoldCommas a)) head appls
        )
    exprOpsParser.TermParser <- parseApply
    let parseFile str = 
        match runParserOnFile (parseDecls.>>eof) () str System.Text.Encoding.ASCII with
        | Success(result, _, _)   -> sprintf "%A" result
        | Failure(errorMsg, _, _) -> errorMsg

