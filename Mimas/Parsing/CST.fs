module CST


open System.Numerics
open MetaInf



type CSTNode<'a> =
    | CSTNode of 'a  * CodeSpan


type CSTExpr =
    | LitVar        of string
    | LitInt        of bigint
    | LitFloat      of double
    | LitString     of string
    | LitFn         of CSTNode<CSTExpr> * (CSTNode<CSTExpr> list)
    | LitId         of string
    | LitSeq        of CSTNode<CSTExpr> list
    | Alt           of CSTNode<CSTExpr> * CSTNode<CSTExpr>
    | AltSeq        of CSTNode<CSTExpr> * CSTNode<CSTExpr>
    | Join          of CSTNode<CSTExpr> * CSTNode<CSTExpr>
    | Thunk         of CSTNode<CSTExpr>
    | GiveType      of CSTNode<CSTExpr> * CSTNode<CSTExpr>
    | Arrow         of CSTNode<CSTExpr> * CSTNode<CSTExpr>
    | Rel           of CSTNode<CSTExpr> * CSTNode<CSTExpr>
    | Assign        of CSTNode<CSTExpr> * CSTNode<CSTExpr>

type CSTStatemenType = 
    | LitDef        of CSTNode<CSTExpr>
    | LitTypeDef    of CSTNode<CSTExpr>
    | LitRule       of CSTNode<CSTExpr>
    | LitRuleType   of CSTNode<CSTExpr>
    | Other         of CSTNode<CSTExpr>


type CSTStatement = CSTStatement of CSTStatemenType * (CSTNode<CSTStatement> list)


let graphNodeMake n name value args =
    let arginc (s,na) _ = (sprintf "%s|a%d" s na,na+1)
    let arglist =
        match args with
        | h::t -> fst (List.fold arginc ("a0",1) t)
        | []   -> ""
    sprintf "   n%d [shape=record,label=\"{<f>|%s|%s{%s}}\"];\n" n name value arglist
let graphEdgesAdd n args =
    let makeEdge (s,nth) a = (sprintf "%s   n%d:a%d -> %s:f\n" s n nth a,nth+1)
    fst (List.fold makeEdge ("",0) args)

