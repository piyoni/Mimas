// For more information see https://aka.ms/fsharp-console-apps
open PAST
open Parsing
open ParseCom
open FParsec
open Token
open Tokenizing

let teststr = "\"hello\" 12343 _as As\n234897493978263 28954734.2"

//printfn "%A\n" (tokenizeFile "programs/test.txt")
//printfn "%A" (tokenize teststr)



let isPlus t =
    match t with
    | TkAdd -> true
    | _     -> false
let prsOp = pipe (fun t -> sprintf "%A" t) advance


let isVar t =
    match t with
    | TkVar _ -> true
    | _       -> false
let prsVar = pipe (fun (Token(TkVar(a),_)) -> a) advance

type Pexpr =
    | Var of string
    | Plus of Pexpr*Pexpr


let parsePexpr = opLeft (fun a (_,c) -> Plus(a,c)) (ParseCase(isPlus,prsOp)) (pipe (fun s->Var(s)) prsVar)


let testst = "A+B+C+Ddfsdfs"
let testtok = tokenize testst
printfn "\n\n"
printfn "%s parses to \n%A\n\n\n" testst (parseTillEnd parsePexpr testtok)
let tester = "A+B+C-Ddfsdfs"
printfn "%s parses to \n%A\n\n\n" tester (parseTillEnd parsePexpr (tokenize tester))