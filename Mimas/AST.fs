module AST
    open System.Numerics
    type CodePos = {
        col:int;
        ln:int;
    }

    type ValType =
        | Integer   of string
        | Floating  of string
        | String    of string
        | Var       of string
        | Ident     of string
    

    type Expr =
        | Const of CodePos * CodePos * ValType
        | App   of CodePos * CodePos * ValType * Expr List
        | Lst   of CodePos * CodePos * Expr List
        | Seq   of CodePos * CodePos * Expr * Expr
        | Alt   of CodePos * CodePos * Expr * Expr
        | Decls of CodePos * CodePos * Decl List
    and Decl =
        | Def of CodePos * CodePos * string * TypeHint * Expr
        | Var of CodePos * CodePos * string * TypeHint * Expr
    and TypeHint =
        | NoHint
        | Hint of Expr

