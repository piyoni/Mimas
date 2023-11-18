module Parser
    open System.Numerics
    open FParsec
    open MetaInf
    type 'a Parser = Parser<'a,unit>

    type ASTNode<'a> = ASTNode of 'a  * CodeSpan

    type ValType =
        | Integer   of string
        | Floating  of string
        | String    of string
        | Var       of string
        | Ident     of string

    type ASTExpr =
        | Val           of ValType
        | Thunk         of ASTNode<ASTExpr>
        | UnOp          of string * ASTNode<ASTExpr>
        | BinOp         of string * ASTNode<ASTExpr> * ASTNode<ASTExpr>
        | Fn            of ASTNode<ASTExpr> * (ASTNode<ASTExpr> list)
        | Seq           of ASTNode<ASTExpr> list
    
    type ASTStatementType = 
        | LitDef        of ASTNode<ASTExpr>
        | LitTypeDef    of ASTNode<ASTExpr>
        | LitRule       of ASTNode<ASTExpr>
        | LitTypeRule   of ASTNode<ASTExpr>
        | Other         of ASTNode<ASTExpr>

    type DefScope       = DefScope of ASTNode<ASTNode<ASTStatement> list>
    and  ASTStatement   = ASTtatement of ASTStatementType * DefScope

    module ParserFunctions =
        let resultSatisfies predicate msg (p: Parser<_,_>) : Parser<_,_> =
            let error = messageError msg
            fun stream ->
                let reply = p stream
                if reply.Status <> Ok || predicate reply.Result then reply
                else
                    Reply(Error, error)
        let parseComment: unit Parser = pstring "#" >>. skipRestOfLine true
        let sep = skipMany (choice [spaces1;parseComment]) .>> optional eof
        // make codeSpan and add Into AST
        let posTocs (ps:Position) (pe:Position) = {
                startcol = (int)ps.Column;
                endcol   = (int)pe.Column;
                startln  = (int)ps.Line;
                endln    = (int)pe.Line;
            }
        let parseNode p = pipe3 getPosition p getPosition (fun st pt ed -> ASTNode(pt, posTocs st ed)) .>> sep
        //
        // parsing different types of value literals
        let parseStr = ((pchar '"') >>. (many1CharsTill (anyChar) (pchar '"'))) |>> ValType.String : ValType Parser
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
        let parseVar : ValType Parser =
            let isAsciiIdStart c =
                isUpper c || c = '_'
            let isAsciiIdContinue c =
                isAsciiLetter c || isDigit c || c = '_'
            identifier (IdentifierOptions(isAsciiIdStart    = isAsciiIdStart, isAsciiIdContinue = isAsciiIdContinue))|>> ValType.Var
        //
        let parseValue : ASTNode<ASTExpr> Parser = parseNode ((choice [parseStr;parseNumber;parseIdent;parseVar])|>> ASTExpr.Val)
        //
        // parsing keywords
        let matchKW str = skipString str .>> sep
        let kwDef  = matchKW "def"
        let kwType = matchKW "type"
        let kwWith = matchKW "with"
        let kwDot  = matchKW "."
        let kwCom  = matchKW ","
        let kwLBr  = matchKW "("
        let kwRBr  = matchKW ")"
        let kwLSq  = matchKW "["
        let kwRSq  = matchKW "]"
        let kwLCu  = matchKW "{"
        let kwRCu  = matchKW "}"
        //
        // operator predecence parsing
        let addInfixOperator (pars:OperatorPrecedenceParser<_,_,_>) str separator prec assoc =
            let op = InfixOperator(str, getPosition.>>separator, prec, assoc, (), (fun p l r -> ASTNode(BinOp(str,l,r),posTocs p p)))
            pars.AddOperator(op)
        let addPrefixOperator (pars:OperatorPrecedenceParser<_,_,_>) str separator prec  =
            let op = PrefixOperator(str, getPosition.>>separator, prec, true, (), (fun p r -> ASTNode(UnOp(str,r),posTocs p p)))
            pars.AddOperator(op)
        let exprOpsParser = OperatorPrecedenceParser()
        (*
        infix_operators {
	    "->" | "--"									: right
	    ";"  | "?" 	| "|"							: right
	    "or" | "xor"								: left
 	    "and"										: left
 	    "<"  | "=" 	| ">" 	| "<>"					: left
 	    "+"  | "-"       							: left
 	    "*"  | "/" 	| "div" | "mod" | "%"			: left
 	    "^" | "`"								    : right
	    "::"										: left
 	    ":"											: left
        }
        prefix_operators {
	    "-"  | "+" 	| "~"
        }
        *)
        // -> --
        addInfixOperator exprOpsParser "->" sep 1 Associativity.Right
        addInfixOperator exprOpsParser "--" sep 1 Associativity.Right
        addInfixOperator exprOpsParser ":=" sep 1 Associativity.Right
        // ; ? |
        addInfixOperator exprOpsParser ";" sep 2 Associativity.Right
        addInfixOperator exprOpsParser "?" sep 2 Associativity.Right
        addInfixOperator exprOpsParser "|" sep 2 Associativity.Right
        // or xor
        addInfixOperator exprOpsParser "or"  sep 3 Associativity.Left
        addInfixOperator exprOpsParser "xor" sep 3 Associativity.Left
        // and
        addInfixOperator exprOpsParser "and" sep 4 Associativity.Left
        // < = > <>
        addInfixOperator exprOpsParser "<" sep 5 Associativity.Left
        addInfixOperator exprOpsParser "=" sep 5 Associativity.Left
        addInfixOperator exprOpsParser ">" sep 5 Associativity.Left
        addInfixOperator exprOpsParser "<>" sep 5 Associativity.Left
        // + -
        addInfixOperator exprOpsParser "+" sep 6 Associativity.Left
        addInfixOperator exprOpsParser "-" sep 6 Associativity.Left
        // * / div mod %
        addInfixOperator exprOpsParser "*"   sep 7 Associativity.Left
        addInfixOperator exprOpsParser "/"   sep 7 Associativity.Left
        addInfixOperator exprOpsParser "div" sep 7 Associativity.Left
        addInfixOperator exprOpsParser "mod" sep 7 Associativity.Left
        addInfixOperator exprOpsParser "%"   sep 7 Associativity.Left
        // ^ `
        addInfixOperator exprOpsParser "^" sep 8 Associativity.Right
        addInfixOperator exprOpsParser "`" sep 8 Associativity.Right
        // ::
        addInfixOperator exprOpsParser "::" sep 9 Associativity.Left
        // :
        addInfixOperator exprOpsParser ":"  sep 10 Associativity.Left
        //
        // parse basic expressions (can be composite)
        let commaSepExprs = sepBy exprOpsParser kwCom
        let parseSeq = parseNode (between kwLSq kwRSq commaSepExprs |>> Seq)
        let parseThunk = parseNode (between kwLCu kwRCu exprOpsParser |>> Thunk)
        let parseBr   =  between kwLBr kwRBr exprOpsParser
        //
        let parseBaseExpr = (choice [parseValue; parseSeq; parseThunk; parseBr])
        let parseApply =
            let parseArgList = (between kwLBr kwRBr commaSepExprs) .>>. getPosition
            let reduceArgs st head appls =
                let fnCombine hd (arg,ed) = ASTNode(ASTExpr.Fn(hd,arg),posTocs st ed)
                List.fold fnCombine head appls
            pipe3 getPosition parseBaseExpr (many parseArgList) reduceArgs
        exprOpsParser.TermParser <- parseApply
        //
        //
        // Parsing statements
        (*
        type ASTStatementType = 
        | LitDef        of ASTNode<ASTExpr> // <def> 		::= "def"  		<expr>
        | LitTypeDef    of ASTNode<ASTExpr> //<type>		::=	"type" 		<expr>
        | LitRule       of ASTNode<ASTExpr> // <rule>		::= 			<expr> "->" <expr>
        | LitRuleType   of ASTNode<ASTExpr> // <typerule>	::= 			<expr> "--" <expr>
        | Other         of ASTNode<ASTExpr> // anything else (error case)
        *)
        let parseDef = kwDef >>. exprOpsParser |>> ASTStatementType.LitDef
        let parseTypedef = kwType >>. exprOpsParser |>> ASTStatementType.LitTypeDef
        let parseRestStatements =
            let statementClassify (ASTNode(expr,span)) =
                match expr with
                | BinOp("->",_,_) -> ASTStatementType.LitRule(ASTNode(expr,span))
                | BinOp("--",_,_) -> ASTStatementType.LitTypeRule(ASTNode(expr,span))
                | _               -> ASTStatementType.Other(ASTNode(expr,span))
            exprOpsParser |>> statementClassify
        let parseStatementType = choice [parseDef; parseTypedef;parseRestStatements] .>> sep
        let parseStatement, parseStatementRef = createParserForwardedToRef()
        let parseLocals =
            let withCase    = parseNode (kwWith >>. (between kwLSq kwRSq (many parseStatement))) |>> DefScope
            let withoutCase = parseNode (notFollowedBy kwWith |>> (fun _ -> [])) |>> DefScope
            choice [withCase;withoutCase]
        let parseStatmentGeneral =
            let parseStatementShape =  parseStatementType .>>. parseLocals |>> ASTStatement.ASTtatement
            parseNode (parseStatementShape .>> kwDot)
        parseStatementRef.Value <- parseStatmentGeneral
        let parseProgramFile = 
            let topLevel = (parseNode (many parseStatement)) |>> DefScope
            topLevel //.>> (sep .>> eof)
    // Get file
    let parseFile str = 
        match runParserOnFile (ParserFunctions.parseProgramFile) () str System.Text.Encoding.ASCII with
        | Success(result, _, _)   -> sprintf "%A" result
        | Failure(errorMsg, _, _) -> errorMsg