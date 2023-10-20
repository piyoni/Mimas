module ParseCom

open System.Numerics
open Token
open MetaInf
open PAST

type ParseResult<'a> = 
    | Parse of Token list * 'a
    | Error of string

type Parser<'a> = (Token list) -> 'a ParseResult
type ParseCase<'a> = ParseCase of (Tok -> bool) * Parser<'a>

let expect (f:Tok->bool) (s: Token list) =
    let fs (Token( tk,_)) = f tk
    fs (s.Head)
let advance (s:Token list) = Parse(s.Tail,s.Head) 
let parserInsert f s = Parse(s,f)
let mkError f (ts:Token list)     = Error(f ts)


let parserApply (f:ParseResult<'a -> 'b>) (a: Parser<'a>) = 
    match f with
    | Error(str)  -> Error(str)
    | Parse(sf,rf) ->
        match a sf with
        | Error(str)  -> Error(str)
        | Parse(sc,ra) -> Parse(sc,rf ra)

let pipe (f: 'a -> 'b) (p: 'a Parser) (s: Token list) = 
    let pf  = parserInsert f s
    parserApply pf p
let pipe2 (f: 'a -> 'b -> 'c) (pa : 'a Parser) (pb : 'b Parser) (s: Token list) = 
    let pf  = parserInsert f s
    let pfa = parserApply pf pa
    parserApply pfa pb
let pipe3 (f: 'a -> 'b -> 'c ->'d) (pa : 'a Parser) (pb : 'b Parser) (pc : 'c Parser) (s: Token list) = 
    let pf  = parserInsert f s
    let pfa = parserApply pf  pa
    let pfb = parserApply pfa pb
    parserApply pfb pc
let pipe4 (f: 'a -> 'b -> 'c ->'d -> 'e) (pa : 'a Parser) (pb : 'b Parser) (pc : 'c Parser) (pd : 'd Parser) (s: Token list) = 
    let pf  = parserInsert f s
    let pfa = parserApply pf  pa
    let pfb = parserApply pfa pb
    let pfc = parserApply pfb pc
    parserApply pfc pd




let rec choice (d: 'a Parser) (cs : ('a ParseCase) list) (s: Token list) = 
    match cs with
    | []                                        -> d s
    | (ParseCase(t,p) :: _) when (expect t s)   -> p s
    | (_::t)                                    -> choice d t s
let defaultError (s: Token list) = mkError (fun ts -> sprintf "found unexpected token [%A]" (ts.Head)) s

let choicewe (cs : ('a ParseCase) list) (s: Token list) = choice defaultError cs s


let opt (ParseCase(c,p): ('a ParseCase)) (s: Token list)=
    if expect c s
    then 
        parserApply (parserInsert Some s) p
    else Parse(s, None)

    
let rec manyrev (acc :'a list) (ParseCase(c,p): ('a ParseCase)) (s: Token list)=
    if expect c s
    then
        match p s with
        | Error(str)  -> Error(str)
        | Parse(sb,res) -> manyrev (res::acc) (ParseCase(c,p)) sb
    else Parse(s, acc)


let many (pc: ('a ParseCase)) (s: Token list) = pipe List.rev (manyrev [] pc) s

let isEOF t =
    match t with
    | TkEOF -> true
    | _     -> false
let parseTillEnd p = pipe2 (fun x _ -> x) p (choicewe [ParseCase(isEOF,advance)])

let opLeft f (ParseCase(opt,opp)) e =
    let popp = ParseCase(opt, (pipe2 (fun x y ->  (x,y)) opp e))
    pipe2 (fun a rest -> List.fold f a rest ) e (many popp)
let opRight f (ParseCase(opt,opp)) e =
    let popp = ParseCase(opt, (pipe2 (fun x y ->  (x,y)) opp e))
    pipe2 (fun a rest -> List.foldBack f rest a ) e (many popp)
let sepBy (ParseCase(opt,opp)) e =
    let popp = ParseCase(opt, (pipe2 (fun x y ->  y) opp e))
    pipe2 (fun a rest ->a::rest) e (many popp)
let parseBetween f op between cl = pipe3 f op between cl
