module Parsing
open System.Numerics
open Token
open MetaInf
open PAST
open ParseCom


let ast1 (f: Tok->'a ) (Token(tk,cs):Token) =
    ASTNode.ASTNode(f tk,(),cs) : ASTNode<'a,unit>
let ast2 (f: Tok -> Tok ->'a ) (Token(tka,csa):Token) (Token(tkb,csb):Token) =
    ASTNode.ASTNode(f tka tkb,(),codeSpanMerge csa csb) : ASTNode<'a,unit>
let ast3 (f: Tok -> Tok -> Tok ->'a ) (Token(tka,csa):Token) (Token(tkb,csb):Token) (Token(tkc,csc):Token)=
    ASTNode.ASTNode(f tka tkb tkc,(),codeSpanMerge (codeSpanMerge csa csb) csc) : ASTNode<'a,unit>

