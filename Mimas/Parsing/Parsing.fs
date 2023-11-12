module Parsing
open Token
open System.Numerics
open FParsec
open MetaInf
open Token
open CST








let first x _ = x


let constrCSTNode posStart node posEnd = CSTNode(node,posTocs posStart posEnd)

let mergeCSTNodeCS (CSTNode(_,csa)) (CSTNode(_,csb)) = codeSpanMerge csa csb
let opCSTNode f left right = CSTNode(f(left,right),mergeCSTNodeCS left right)
let cstNodeSpan (CSTNode(_,cs)) = cs




let infixOperatorNodeFunction str pos left right =
    let sourceRange = mergeCSTNodeCS left right
    let head = CSTNode(CSTExpr.LitId(str),posTocs pos pos)
    CSTNode(CSTExpr.LitFn(head,[left;right]),sourceRange)
let prefixOperatorNodeFunction str pos right =
    let sourceRange = mergeCSTNodeCS right right
    let head = CSTNode(CSTExpr.LitId(str),posTocs pos pos)
    CSTNode(CSTExpr.LitFn(head,[right]),sourceRange)

let addInfixOperator (pars:OperatorPrecedenceParser<_,_,_>) str separator prec assoc f =
    let op = InfixOperator(str, getPosition.>>separator, prec, assoc, (), f str)
    pars.AddOperator(op)
let addPrefixOperator (pars:OperatorPrecedenceParser<_,_,_>) str separator prec assoc =
    let op = PrefixOperator(str, getPosition.>>separator, prec, assoc, (), prefixOperatorNodeFunction str)
    pars.AddOperator(op)



let addOps1 p =
    addInfixOperator p "->"     sep 1 Associativity.Right (fun _ _ left right -> opCSTNode CSTExpr.Arrow left right)
    addInfixOperator p "--"     sep 1 Associativity.Right (fun _ _ left right -> opCSTNode CSTExpr.Rel left right)
let addOps2 p =
    addInfixOperator p ";"      sep 2 Associativity.Right (fun _ _ left right -> opCSTNode CSTExpr.Join    left right)
    addInfixOperator p "?>"     sep 2 Associativity.Right (fun _ _ left right -> opCSTNode CSTExpr.AltSeq left right)
    addInfixOperator p "?"      sep 2 Associativity.Right (fun _ _ left right -> opCSTNode CSTExpr.Alt     left right)
let addOps3 p =
    addInfixOperator p ":="     sep 3 Associativity.Right (fun _ _ left right -> opCSTNode CSTExpr.Assign    left right)
let addOps4 p =
    addInfixOperator p "|"      sep 4 Associativity.Right infixOperatorNodeFunction
let addOps5 p =
    addInfixOperator p "xor"    idsep 5 Associativity.Left infixOperatorNodeFunction
    addInfixOperator p "or"     idsep 5 Associativity.Left infixOperatorNodeFunction
let addOps6 p =
    addInfixOperator p "and"    idsep 6 Associativity.Left infixOperatorNodeFunction
let addOps7 p =
    addInfixOperator p "=<"     sep 7 Associativity.Left infixOperatorNodeFunction
    addInfixOperator p ">="     sep 7 Associativity.Left infixOperatorNodeFunction
    addInfixOperator p "<>"     sep 7 Associativity.Left infixOperatorNodeFunction
    addInfixOperator p ">"      sep 7 Associativity.Left infixOperatorNodeFunction
    addInfixOperator p "<"      sep 7 Associativity.Left infixOperatorNodeFunction
    addInfixOperator p "="      sep 7 Associativity.Left infixOperatorNodeFunction
let addOps8 p =
    addInfixOperator p "+" sep 8 Associativity.Left infixOperatorNodeFunction
    addInfixOperator p "-" sep 8 Associativity.Left infixOperatorNodeFunction
    addInfixOperator p "`" sep 8 Associativity.Left infixOperatorNodeFunction
let addOps9 p =
    addInfixOperator p "*" sep 8 Associativity.Left infixOperatorNodeFunction
    addInfixOperator p "/" sep 8 Associativity.Left infixOperatorNodeFunction
    addInfixOperator p "%" sep 8 Associativity.Left infixOperatorNodeFunction
    addInfixOperator p "mod" idsep 8 Associativity.Left infixOperatorNodeFunction
    addInfixOperator p "rem" idsep 8 Associativity.Left infixOperatorNodeFunction
let addOps10 p =
    addInfixOperator p "^" sep 10 Associativity.Right infixOperatorNodeFunction
let addOpsPre p = 
    addPrefixOperator p "-" sep 11 true
    addPrefixOperator p "+" sep 11 true
    addPrefixOperator p "~" sep 11 true


// bracketed things
let parseThunk p f = between tokLCU tokRCU p |>> (fun (CSTNode(x,cs)) -> CSTNode(f (CSTNode(x,cs)),cs))
let parseBr    p   = between tokLBR tokRBR p

let parseSeq p f = 
    let args = sepBy p (tokCOM)
    pipe3 getPosition (between tokLSQ tokRSQ args) getPosition (fun ps ls pe -> CSTNode(f ls,posTocs ps pe))

let parseArgList p =
    let args = sepBy p (tokCOM)
    between tokLBR tokRBR args
//

let parseValue = 
    let tokVal = tokWithSpan (choice [
        tokIdent;
        tokVar;
        tokNumber;
        tokStr
        ])
    let valToNode v=
        match v with
        | ValType.Integer(i) -> CSTExpr.LitInt(i)
        | ValType.Floating(f) -> CSTExpr.LitFloat(f)
        | ValType.String(s) -> CSTExpr.LitString(s)
        | ValType.Var(vv) -> CSTExpr.LitVar(vv)
        | ValType.Ident(i) -> CSTExpr.LitId(i)
    let cstValNode (valtype,cs) =
        CSTNode(valToNode valtype,cs)
    tokVal |>> cstValNode
        
//type ValType =
//    | Integer   of bigint
//    | Floating  of float
//    | String    of string
//    | Var       of string
//    | Ident     of string
// parse expressions

let exprOpsParser = OperatorPrecedenceParser()
addOps1   exprOpsParser
addOps2   exprOpsParser
addOps3   exprOpsParser
addOps4   exprOpsParser
addOps5   exprOpsParser
addOps6   exprOpsParser
addOps7   exprOpsParser
addOps8   exprOpsParser
addOps9   exprOpsParser
addOps10  exprOpsParser


addOpsPre exprOpsParser

let parseBaseExpr p = 
    let terms = choice [
        parseSeq   p CSTExpr.LitSeq;
        parseBr    p;
        parseThunk p CSTExpr.Thunk;
        parseValue
    ]
    let combineCalls head calls =
        let argsCS a = List.fold (fun cs0 (CSTNode(_,cs1)) -> codeSpanMerge cs0 cs1) codeSpanNull a
        let litFnconstructor h a =
            CSTNode(CSTExpr.LitFn(h,a),codeSpanMerge (cstNodeSpan h) (argsCS a))
        List.fold litFnconstructor head calls
    let parseCall =
        pipe2 terms (many (parseArgList p)) combineCalls
    parseCall
exprOpsParser.TermParser <- parseBaseExpr exprOpsParser


//type ASTStatemenType<'b> = 
//    | LitDef        of string * (ASTNode<'b ASTType,'b> list   ) Option* ASTNode<'b ASTType,'b>
//    | LitTypeDef    of string * (ASTNode<'b ASTType,'b> list   ) Option
//    | LitRule       of string * (ASTNode<'b ASTPattern,'b> list) Option* ASTNode<'b ASTExpr,'b>
//    | LitRuleType   of string * (ASTNode<'b ASTType,'b> list   ) Option* ASTNode<'b ASTType,'b>
let parseLitDef =
    let parseInternal = tokDef >>. exprOpsParser |>> fun e -> CSTStatemenType.LitDef(e)
    pipe3 getPosition parseInternal getPosition constrCSTNode

let parseLitTypeDef =
    let parseInternal = tokType >>. exprOpsParser  |>> fun e -> CSTStatemenType.LitTypeDef(e)
    pipe3 getPosition parseInternal getPosition constrCSTNode

let parseLitRuleTypeRuleOther =
    let exprClass (CSTNode(e,cs)) =
        match e with
        | CSTExpr.Arrow(h,b) -> CSTStatemenType.LitRule(CSTNode(e,cs))
        | CSTExpr.Rel(h,b) -> CSTStatemenType.LitRuleType(CSTNode(e,cs))
        | _ -> CSTStatemenType.Other(CSTNode(e,cs))
    let parseInternal = exprOpsParser |>> exprClass
    (pipe3 getPosition parseInternal getPosition constrCSTNode)


let parseStatementType = choice [parseLitDef; parseLitTypeDef;parseLitRuleTypeRuleOther]

let parseStatement, parseStatementRef = createParserForwardedToRef()

let rec parseStatementCST =
    let pst (CSTNode(st,cs)) x =
        match x with
        | None    -> CSTNode(CSTStatement(st,[]),cs)
        | Some(s) -> CSTNode(CSTStatement(st,s),cs)
    (pipe2 parseStatementType (opt (tokWith >>. tokLSQ >>. (many parseStatement) .>> tokRSQ)) pst) .>> tokSTEND



let parseFile str = 
    match runParserOnFile ((many parseStatementCST) .>> eof) () str System.Text.Encoding.ASCII with
    | Success(result, _, _)   -> sprintf "%A" result
    | Failure(errorMsg, _, _) -> errorMsg
let parseFileAST str = 
    match runParserOnFile ((many parseStatementCST) .>> eof) () str System.Text.Encoding.ASCII with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, _, _) -> []