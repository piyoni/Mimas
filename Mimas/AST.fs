module AST
    open System.Numerics
    type CodePos = {
        col:int;
        ln:int;
    }
    type CodeSpan = CodeSpan of CodePos * CodePos

    type ValType =
        | Integer   of string
        | Floating  of string
        | String    of string
        | Var       of string
        | Ident     of string
        | Operat    of string
    

    type Expr =
        | Const of CodeSpan * ValType
        | App   of CodeSpan * ValType * Expr List
        | Cal   of CodeSpan * Expr * Expr List
        | Lst   of CodeSpan * Expr List
        | Seq   of CodeSpan * Expr * Expr
        | Alt   of CodeSpan * Expr * Expr
        | Fn    of CodeSpan * Expr * Expr
        | FnIf  of CodeSpan * Expr * Expr
        | Com   of CodeSpan * Expr * Expr
        | Decls of CodeSpan * Decl List
    and Decl =
        | Def of CodeSpan * ValType * Option<Expr> * Expr
        | Var of CodeSpan * Expr   * Option<Expr> * Expr

