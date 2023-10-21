// For more information see https://aka.ms/fsharp-console-apps
open PAST
open FParsec
open Token
open Parsing

let teststr = "\"hello\" 12343 _as As\n234897493978263 28954734.2"

printfn "%A\n" (parseFile "programs/test.txt")


//printfn "%s" (parseExpr  "(A and not(B)) or (not(A) and B)")