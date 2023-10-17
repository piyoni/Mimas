module Parsing
open System.Numerics
open Token
open MetaInf
open PAST
open ParseCom


let ast1 (f: 'a->'a ) (ASTNode(asta,(),csa)) =
    ASTNode.ASTNode(f asta,(),csa) : ASTNode<'a,unit>
let ast2 (f: 'a -> 'a -> 'a ) (ASTNode(asta,(),csa))  (ASTNode(astb,(),csb)) =
    ASTNode.ASTNode(f asta astb ,() ,codeSpanMerge csa csb) : ASTNode<'a,unit>
let ast3 (f: 'a -> 'a -> 'a -> 'a ) (ASTNode(asta,(),csa))  (ASTNode(astb,(),csb)) (ASTNode(astc,(),csc)) =
    ASTNode.ASTNode(f asta astb astc ,() ,codeSpanMerge (codeSpanMerge csa csb) csc) : ASTNode<'a,unit>
let ast4 (f: 'a -> 'a -> 'a -> 'a -> 'a ) (ASTNode(asta,(),csa))  (ASTNode(astb,(),csb)) (ASTNode(astc,(),csc)) (ASTNode(astd,(),csd)) =
    ASTNode.ASTNode(f asta astb astc astd ,() ,codeSpanMerge (codeSpanMerge csa csb) (codeSpanMerge csc csd)) : ASTNode<'a,unit>



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
