module Parsing
open System.Numerics
open Token
open MetaInf
open PAST
open ParseCom


let ast1 (f: 'a->'b ) (ASTNode(asta,(),csa)) =
    ASTNode.ASTNode(f asta,(),csa) : ASTNode<'b,unit>
let ast2 (f: 'a -> 'b -> 'c ) (ASTNode(asta,(),csa))  (ASTNode(astb,(),csb)) =
    ASTNode.ASTNode(f asta astb ,() ,codeSpanMerge csa csb) : ASTNode<'c,unit>
let ast3 (f: 'a -> 'b -> 'c -> 'd ) (ASTNode(asta,(),csa))  (ASTNode(astb,(),csb)) (ASTNode(astc,(),csc)) =
    ASTNode.ASTNode(f asta astb astc ,() ,codeSpanMerge (codeSpanMerge csa csb) csc) : ASTNode<'d,unit>
let ast4 (f: 'a -> 'b -> 'c -> 'd -> 'e ) (ASTNode(asta,(),csa))  (ASTNode(astb,(),csb)) (ASTNode(astc,(),csc)) (ASTNode(astd,(),csd)) =
    ASTNode.ASTNode(f asta astb astc astd ,() ,codeSpanMerge (codeSpanMerge csa csb) (codeSpanMerge csc csd)) : ASTNode<'e,unit>
let astBetween (f: 'a -> 'b -> 'c -> 'd ) (ASTNode(asta,(),csa))  middle (ASTNode(astc,(),csc)) =
    ASTNode.ASTNode(f asta middle astc ,() ,codeSpanMerge csa csc) : ASTNode<'d,unit>


let pTok = (pipe (fun (Token(a,_))-> a)) advance

let isInt t = 
    match t with
    | TkInt _ -> true
    | _       -> false
let isFloat t = 
    match t with
    | TkFloat _ -> true
    | _       -> false
let isString t = 
    match t with
    | TkString _ -> true
    | _       -> false
let isIdent t = 
    match t with
    | TkIdent _ -> true
    | _       -> false
let isVar t =
    match t with
    | TkVar _ -> true
    | _       -> false

let fExprVar t = 
    match t with
    | Token(TkVar(a),cs) -> ASTNode((ASTExpr.Var(a):ASTExpr<unit>),(),cs)
    | _ -> raise (invalidArg "t" "invalid ExpressionVar")
let fTypeVar t = 
    match t with
    | Token(TkVar(a),cs) -> ASTNode((ASTType.Var(a):ASTType<unit>),(),cs)
    | _ -> raise (invalidArg "t" "invalid TypeVar")
let fPatternVar t = 
    match t with
    | Token(TkVar(a),cs) -> ASTNode((ASTPattern.Var(a):ASTPattern<unit>),(),cs)
    | _ -> raise (invalidArg "t" "invalid PatternVar")

let parseExprVar    = pipe fExprVar advance
let parseTypeVar    = pipe fTypeVar advance
let parsePatternVar = pipe fPatternVar advance

let fExprInt t = 
    match t with
    | Token(TkInt(a),cs) -> ASTNode((ASTExpr.LitInt(a):ASTExpr<unit>),(),cs)
    | _ -> raise (invalidArg "t" "invalid ExpressionInt")
let fPatternInt t = 
    match t with
    | Token(TkInt(a),cs) -> ASTNode((ASTPattern.LitInt(a):ASTPattern<unit>),(),cs)
    | _ -> raise (invalidArg "t" "invalid PatternInt")

let parseExprInt    = pipe fExprInt advance
let parsePatternInt = pipe fPatternInt advance

let fExprFloat t = 
    match t with
    | Token(TkFloat(a),cs) -> ASTNode((ASTExpr.LitFloat(a):ASTExpr<unit>),(),cs)
    | _ -> raise (invalidArg "t" "invalid ExpressionFloat")
let fPatternFloat t = 
    match t with
    | Token(TkFloat(a),cs) -> ASTNode((ASTPattern.LitFloat(a):ASTPattern<unit>),(),cs)
    | _ -> raise (invalidArg "t" "invalid PatternFloat")

let parseExprFloat    = pipe fExprFloat advance
let parsePatternFloat = pipe fPatternFloat advance

let fExprString t = 
    match t with
    | Token(TkString(a),cs) -> ASTNode((ASTExpr.LitString(a):ASTExpr<unit>),(),cs)
    | _ -> raise (invalidArg "t" "invalid ExpressionString")
let fPatternString t = 
    match t with
    | Token(TkString(a),cs) -> ASTNode((ASTPattern.LitString(a):ASTPattern<unit>),(),cs)
    | _ -> raise (invalidArg "t" "invalid PatternString")

let parseExprString    = pipe fExprString advance
let parsePatternString = pipe fPatternString advance

let expectToken tk = choicewe [ParseCase((fun t -> t=tk),(pipe (fun (Token(_,cs)) -> ASTNode((),(),cs)) advance))]


let parseInParens   p = pipe3 (ast3 (fun _ b _ -> b)) (expectToken TkLBr) p (expectToken TkRBr)
let parseInCurly    p = pipe3 (ast3 (fun _ b _ -> b)) (expectToken TkLCu) p (expectToken TkRCu)


let isComma t = 
    match t with
    | TkCom -> true
    | _     -> false
let parseCommaSep e = sepBy (ParseCase(isComma,pipe (fun _ x -> x) advance)) e

let parseArgList e = pipe3 (astBetween (fun _ b _ -> b)) (expectToken TkLSq)  (parseCommaSep e) (expectToken TkRSq) 
let parseSeqList p = pipe3 (astBetween (fun _ b _ -> b)) (expectToken TkLSq) (parseCommaSep p) (expectToken TkRSq)


//<op0> 	::= ";"  | "?" | "?>"
let isOp0 t =
    match t with
    | TkSemi -> true
    | TkQue  -> true
    | TkSequ -> true
    | _      -> false
let parseOps0 f = opRight f (ParseCase(isOp0,pTok))
//<op1> 	::= "|"
let isOp1 t =
    match t with
    | TkPipe -> true
    | _      -> false
let parseOps1 f = opRight f (ParseCase(isOp1,pTok))
//<op2> 	::= "or" | "xor"
let isOp2 t =
    match t with
    | TkOr   -> true
    | TkXor  -> true
    | _      -> false
let parseOps2 f = opLeft f (ParseCase(isOp2,pTok))
//<op3> 	::= "and"
let isOp3 t =
    match t with
    | TkPipe -> true
    | _      -> false
let parseOps3 f = opLeft f (ParseCase(isOp3,pTok))
//<op4> 	::= "<"  | "=" 	| ">" 	| "<>" | ">=" | "=<"
let isOp4 t =
    match t with
    | TkPipe -> true
    | _      -> false
let parseOps4 f = opLeft f (ParseCase(isOp4,pTok))
//<op5> 	::= "+"  | "-" | "`"
let isOp5 t =
    match t with
    | TkAdd  -> true
    | TkSub  -> true
    | TkConc -> true
    | _      -> false
let parseOps5 f = opLeft f (ParseCase(isOp5,pTok))
//<op6> 	::= "*"  | "/" 	| "mod" | "%" | "rem"
let isOp6 t =
    match t with
    | TkMul     -> true
    | TkDiv     -> true
    | TkMod     -> true
    | TkPercent -> true
    | TkRem     -> true
    | _         -> false
let parseOps6 f = opLeft f (ParseCase(isOp6,pTok))
//<op7> 	::= "^"
let isOp7 t =
    match t with
    | TkPow -> true
    | _      -> false
let parseOps7 f = opRight f (ParseCase(isOp7,pTok))
//<preop> ::= "-"  | "+" 	| "~"
let isPreOp t =
    match t with
    | TkSub -> true
    | TkAdd -> true
    | TkNot -> true
    | _      -> false
let preopreduce f (Token(tk,csa)) (ASTNode(e,(),cse):ASTNode<'a,unit>) = ASTNode(f tk,(),codeSpanMerge csa cse)
let parseOpsPre f e = pipe2 (fun ops er -> List.foldBack (preopreduce f) ops er) (many (ParseCase(isPreOp,advance))) e


//<basetype> ("::" <type>)?
let parseTypeHint f e t =
    let parseOptType = opt (ParseCase((fun tk -> tk=TkTypeHint),pipe2 (fun _ y -> y) advance t))
    let ifcombine a u =
        match u with
        | None -> a
        | Some(eb) -> ast2 f a eb
    pipe2 ifcombine e parseOptType

//"(" <type> ("->" <type>)? ")"
let parseArrow f ea t =
    let parseOptArrow = opt (ParseCase((fun tk -> tk=TkFn),pipe2 (fun _ y -> y) advance t))
    let ifcombine a u =
        match u with
        | None -> a
        | Some(eb) -> ast2 f a eb
    pipe2 ifcombine ea parseOptArrow

//<identifier> ("("<typelist>")")? 
let parseTerm f ea t =
    let parseOptArrow = opt (ParseCase((fun tk -> tk=TkLBr),parseArgList ea))
    let ifcombine a u =
        match u with
        | None -> a
        | Some(eb) -> ast2 f a eb
    pipe2 ifcombine ea parseOptArrow


let typeOpCons op (ASTNode(e0,(),cs0)) (ASTNode(e1,(),cs1))= 
    let fhead = tkStr op
    let exp = ASTType.LitFn(fhead,(Some([(ASTNode(e0,(),cs0));(ASTNode(e0,(),cs1))])))
    ASTNode(exp,(),codeSpanMerge cs0 cs1)

let rec parseType = parseOps0 (fun (op,e0) e1 -> typeOpCons op e0 e1) parseType1
and parseType1 = parseOps1 (fun (op,e0) e1 -> typeOpCons op e0 e1) parseTypeTypeHint
and parseTypeTypeHint = parseTypeHint (fun th tt -> th) parseBaseType parseBaseType
and parseBaseType = choicewe []