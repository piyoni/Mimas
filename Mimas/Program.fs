// For more information see https://aka.ms/fsharp-console-apps
open FParsec
open Parser
let teststr = "\"hello\" 12343 _as As\n234897493978263 28954734.2"

printfn "%A\n" (parseFile "programs/test.txt")

//printfn "%s\n\n" (graphNodeMake 0 "test" "hello" [])
//printfn "%s" (graphEdgesAdd 2 ["n4";"n5";"n120"])
//printfn "%s" (parseExpr  "(A and not(B)) or (not(A) and B)")