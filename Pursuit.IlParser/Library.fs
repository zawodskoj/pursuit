module IlParser

open FParsec

type CilInsn =
    | Ldstr of string
    | Call of asm: string * tpe: string * methn: string * rettpe: string * argv: string list
    | Ret
    member x.TryCall(asm: string outref, tpe: string outref, methn: string outref, rettpe: string outref, argv: string array outref) =
        match x with
        | Call (_asm, _tpe, _methn, _rettpe, _argv) ->
            asm <- _asm
            tpe <- _tpe
            methn <- _methn
            rettpe <- _rettpe
            argv <- _argv |> List.toArray
            true
        | _ -> false
    member x.TryLdstr(str: string outref) =
        match x with
        | Ldstr s -> str <- s; true
        | _ -> false
    member x.TryRet() =
        match x with
        | Ret -> true
        | _ -> false

let ident =
    let startChar c = c = '_' || isAsciiLetter c
    let nextChar c = startChar c || isDigit c
    identifier (IdentifierOptions (startChar, nextChar))
    
let isWs = function
| ' ' | '\t' -> true
| _ -> false

let ws = skipManySatisfy isWs
    
let ws1 = skipMany1Satisfy isWs
    
let parseRet = ws >>. pstring "ret" >>. ws >>. preturn Ret

let parseLdstr =
    // stupid impl
    let strLit =
        let okchr = function
        | '"' -> false
        | '\\' -> false // there's no escape
        | '\n' -> false
        | _ -> true
            
        pchar '"' >>. many1Satisfy okchr .>> pchar '"'
        
    ws >>. pstring "ldstr" >>. ws1 >>. strLit .>> ws |>> Ldstr
    
let parseCall =
    let assembly = between (pchar '[') (pchar ']') (ws >>. ident .>> ws)
    let tpe = sepBy1 ident (attempt (ws >>. pchar '.' >>. ws)) |>> String.concat "."
    let argv =
        let argv' = sepBy tpe (attempt (ws >>. pchar ',' >>. ws))
        between (pchar '(') (pchar ')') (ws >>. argv' .>> ws)
    
    let unwrap ((((retTpe, asm), tpe), method), argv) = Call (asm, tpe, method, retTpe, argv)  
    
    ws >>. pstring "call" >>. ws1 >>. ident .>> ws .>>. assembly
        .>> ws .>>. tpe .>> ws .>> pstring "::" .>> ws .>>. ident .>> ws .>>. argv .>> ws
        |>> unwrap
    
let attemptChoice l = choice (List.map attempt l)    
let parseInsn = ws >>. choice [
    followedBy (pstring "ret") >>. parseRet
    followedBy (pstring "ldstr") >>. parseLdstr
    followedBy (pstring "call") >>. parseCall
]

let parseInsns =
    let parseInsnOrNothing = ws >>. choice [
        notFollowedBy (skipNewline <|> eof) >>. parseInsn .>> ws |>> List.singleton
        preturn []
    ]
    
    (sepBy parseInsnOrNothing newline) .>> ws .>> eof |>> List.concat
    
let parseCilCode str =
    match run parseInsns str with
    | Success (a, _, _) -> a
    | Failure (f, _,  _) -> [Ldstr f]
    // why handle failures