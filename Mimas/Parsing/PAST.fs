module PAST

open System.Numerics
open MetaInf



type ASTNode<'a,'b> =
    | ASTNode of 'a * 'b * CodeSpan

//<type>  ::= <type1> (<op0> <type1>)*
//<type1> ::= <type2> (<op1> <type2>)*
//<type2> ::= <basetype> ("^" (<int>|<var>))?
//<basetype>  ::= <identifier> ("("<typelist>")")? ("::" <type>)?
//	| 	"(" <type> ("->" <type>)? ")"
//	|   "{" <type> "}"
//	|	"[" <typelist> "]"

let ppcommasep = String.concat ","
type ASTType<'b> =
    | Var           of string
    | LitFn         of string * (ASTNode<('b ASTType), 'b>  list)
    | LitId         of string
    | LitSeq        of ASTNode<('b ASTType), 'b> list
    | Thunk         of ASTNode<'b ASTType,'b>
    | Alt           of ASTNode<'b ASTType, 'b> * ASTNode<'b ASTType, 'b>
    | Join          of ASTNode<'b ASTType, 'b> * ASTNode<'b ASTType, 'b>
    | Pow           of ASTNode<'b ASTType, 'b> * bigint
    | GiveType      of ASTNode<'b ASTType, 'b> * ASTNode<'b ASTType, 'b>
let rec ppasttype ast =
    match ast with
    | Var(s)                                    -> s
    | LitFn(s,args)                             -> sprintf "%s(%s)" s (ppcommasep (List.map (fun (ASTNode(a,_,_))-> ppasttype a) args))
    | LitId(s)                                  -> s
    | LitSeq(args)                              -> sprintf "[%s]" (ppcommasep (List.map (fun (ASTNode(a,_,_))-> ppasttype a) args))
    | Thunk(ASTNode(a,_,_))                     -> sprintf "{%s}" (ppasttype a)
    | Alt(ASTNode(a,_,_),ASTNode(b,_,_))        -> sprintf "%s ? %s" (ppasttype a) (ppasttype b)
    | Join(ASTNode(a,_,_),ASTNode(b,_,_))       -> sprintf "%s ; %s" (ppasttype a) (ppasttype b)
    | Pow(ASTNode(a,_,_),v)                     -> sprintf "(%s ^ %A)" (ppasttype a) v
    | GiveType(ASTNode(a,_,_),ASTNode(b,_,_))   -> sprintf "(%s :: %s)" (ppasttype a) (ppasttype b)


//expr>  	::= <expr1> 	(<op0> <expr1>  )*
//<exprpre> 	::= <preop>* (<baseexpr> ("::" <type>)?)
//<baseexpr> ::= <value> 
//	| 	"(" <expr> ("->" <expr>)? ")"
//	|   "{" <expr> "}"
//	|	"[" <exprlist> "]"
//	|	<identifier> ("("<exprlist>")")?
//	| 	<variable>
type ASTExpr<'b> =
    | Var           of string
    | LitInt        of bigint
    | LitFloat      of double
    | LitString     of string
    | LitFn         of string * (ASTNode<'b ASTExpr,'b> list)
    | LitId         of string
    | LitSeq        of ASTNode<'b ASTExpr,'b> list
    | Alt           of ASTNode<'b ASTExpr,'b> * ASTNode<'b ASTExpr,'b>
    | Join          of ASTNode<'b ASTExpr,'b> * ASTNode<'b ASTExpr,'b>
    | Thunk         of ASTNode<'b ASTExpr,'b>
    | GiveType      of ASTNode<'b ASTExpr,'b> * ASTNode<'b ASTType,'b>
let rec ppastexpr ast =
    match ast with
    | Var(s)                                    -> s
    | LitFn(s,args)                             -> sprintf "%s(%s)" s (ppcommasep (List.map (fun (ASTNode(a,_,_))-> ppastexpr a) args))
    | LitId(s)                                  -> s
    | LitInt(i)                                 -> sprintf "%A" i
    | LitFloat(f)                               -> sprintf "%f" f
    | LitString(s)                              -> sprintf "%s" s
    | LitSeq(args)                              -> sprintf "[%s]" (ppcommasep (List.map (fun (ASTNode(a,_,_))-> ppastexpr a) args))
    | Thunk(ASTNode(a,_,_))                     -> sprintf "{%s}" (ppastexpr a)
    | Alt(ASTNode(a,_,_),ASTNode(b,_,_))        -> sprintf "%s ? %s" (ppastexpr a) (ppastexpr b)
    | Join(ASTNode(a,_,_),ASTNode(b,_,_))       -> sprintf "%s ; %s" (ppastexpr a) (ppastexpr b)
    | GiveType(ASTNode(a,_,_),ASTNode(b,_,_))   -> sprintf "(%s :: %s)" (ppastexpr a) (ppasttype b)
//<def> 		::= "def"  		<identifier> ("("<typelist>")")? "->" <type> <localdefs>?
//<rule>		::= 			<pattern> "->" <type> <localdefs>?

//<type>		::=	"type" 		<identifier> ("("<typelist>")")?  <localdefs>?
//<typerule>	::= 			<type> "--" <type> <localdefs>?

//<statement> ::= (<def>|<rule>|<type>|<typerule>) "."

type ASTStatemenType<'b> = 
    | LitDef        of string * (ASTNode<'b ASTType,'b> list   ) Option * ASTNode<'b ASTType,'b>
    | LitTypeDef    of string * (ASTNode<'b ASTType,'b> list   ) Option
    | LitRule       of ASTNode<'b ASTExpr,'b> * ASTNode<'b ASTExpr,'b>
    | LitRuleType   of ASTNode<'b ASTType,'b> * ASTNode<'b ASTType,'b>
let ppastOptlist f ast = 
    match ast with
    | None     -> ""
    | Some(ls) -> sprintf "(%s)" (ppcommasep (List.map (fun (ASTNode(a,_,_))-> f a) ls))
let ppaststatementtype ast =
    match ast with
    | LitDef(head,args,ASTNode(body,_,_)) -> sprintf "def %s %s -> %s" head (ppastOptlist ppasttype args) (ppasttype body)
    | LitTypeDef(head,args) -> sprintf "type %s %s " head (ppastOptlist ppasttype args)
    | LitRule(ASTNode(head,_,_),ASTNode(body,_,_)) -> sprintf "rule %s -> %s" (ppastexpr head) (ppastexpr body)
    | LitRuleType(ASTNode(head,_,_),ASTNode(body,_,_)) -> sprintf "rule %s -- %s" (ppasttype head) (ppasttype body)



type ASTStatement<'b> = ASTStatement of 'b ASTStatemenType * (ASTNode<'b ASTStatement, 'b> list)
let rec ppaststatmentpre pre (ASTStatement(stt,ls)) =
    let argstrs = List.map (fun (ASTNode(a,_,_))-> ppaststatmentpre (pre+"   ") a) ls
    sprintf "%s%s\n%s[\n%s]" pre (ppaststatementtype stt) pre (String.concat "\n" argstrs)
let ppaststatment ast = ppaststatmentpre "" ast

let ppaststatments asts = String.concat "\n" (List.map (fun (ASTNode(a,_,_))-> ppaststatment a) asts)