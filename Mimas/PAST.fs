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
type ASTType<'b> =
    | Var           of string
    | LitFn         of string * (ASTNode<('b ASTType), 'b>  list) Option
    | Thunk         of ASTNode<'b ASTType, 'b>
    | Seq           of ASTNode<('b ASTType), 'b> list
    | Alt           of ASTNode<'b ASTType, 'b> * ASTNode<'b ASTType, 'b>
    | Join          of ASTNode<'b ASTType, 'b> * ASTNode<'b ASTType, 'b>
    | Pow           of ASTNode<'b ASTType, 'b> * bigint
    | GiveType      of ASTNode<'b ASTType, 'b> * ASTNode<'b ASTType, 'b>


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
    | LitFn         of string * (ASTNode<'b ASTExpr,'b> list) Option
    | LitSeq        of ASTNode<'b ASTExpr,'b> list
    | Alt           of ASTNode<'b ASTExpr,'b> * ASTNode<'b ASTExpr,'b>
    | Join          of ASTNode<'b ASTExpr,'b> * ASTNode<'b ASTExpr,'b>
    | Thunk         of ASTNode<'b ASTExpr,'b>
    | GiveType      of ASTNode<'b ASTExpr,'b> * ASTNode<'b ASTType,'b>
   

//<pattern>  	::= <pattern1> 	(<op0> <pattern1>  )*
//<pattern7> 	::= <patternpre>(<op7> <patternpre>)*
//<patternpre>::= <preop>* <basepattern>

//<basepattern> ::= <value> 
//	| 	"(" <pattern> ("->" <pattern>)? ")"
//	|   "{" <pattern> "}"
//	|	"[" <patternlist> "]"
//	|	<identifier> ("("<patternlist>")")?
//	| 	<variable>
type ASTPattern<'b> =
    | Var           of string
    | LitInt        of bigint
    | LitFloat      of double
    | LitString     of string
    | LitFn         of string * (ASTNode<'b ASTPattern, 'b> list) Option
    | LitSeq        of ASTNode<'b ASTPattern, 'b> list
    | Thunk         of ASTNode<'b ASTPattern, 'b>


//<def> 		::= "def"  		<identifier> ("("<typelist>")")? "->" <type> <localdefs>?
//<rule>		::= 			<pattern> "->" <type> <localdefs>?

//<type>		::=	"type" 		<identifier> ("("<typelist>")")?  <localdefs>?
//<typerule>	::= 			<type> "--" <type> <localdefs>?

//<statement> ::= (<def>|<rule>|<type>|<typerule>) "."

type ASTStatemenType<'b> = 
    | LitDef        of string * (ASTNode<'b ASTType,'b> list   ) * ASTNode<'b ASTType,'b>
    | LitTypeDef    of string * (ASTNode<'b ASTType,'b> list   ) * ASTNode<'b ASTType,'b>
    | LitRule       of string * (ASTNode<'b ASTPattern,'b> list) * ASTNode<'b ASTExpr,'b>
    | LitRuleType   of string * (ASTNode<'b ASTType,'b> list   ) * ASTNode<'b ASTType,'b>
type ASTStatement<'b> = ASTStatement of 'b ASTStatemenType * (ASTNode<'b ASTStatement, 'b> list)

// need combinators for cases
// expect 
// lookahead
// 
