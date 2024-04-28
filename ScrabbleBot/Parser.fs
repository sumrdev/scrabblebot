// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the module Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many whitespaceChar <?> "spaces"
    let spaces1        = many1 whitespaceChar <?> "space1"

    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2
    let (.>*>) p1 p2  = p1 .>> spaces .>> p2
    let (>*>.) p1 p2  = p1 >>. spaces >>. p2

    let parenthesise p =  pchar '(' >*>. p .>*> pchar ')'
    let pcurlybrackets p =  pchar '{' >*>. p .>*> pchar '}'
    let psquarebrackets p =  pchar '[' >*>. p .>*> pchar ']'
    let psinglequotes p =  pchar ''' >>. p .>> pchar '''
    let charListToString charList = charList |> Array.ofList |> System.String
    let pid = 
        let firstChar = pletter <|> pchar '_'
        let rest = many (palphanumeric <|> pchar '_')
        //convert the char list to a string
        firstChar .>>. rest |>> (fun (c, cs) -> c::cs |> List.toArray |> System.String)
    
    let binop op a b = a .>*> op .>*>. b

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    
    let AexpParse = TermParse 
    let CexpParse, cref = createParserForwardedToRef<cExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Add"
    do tref := choice [AddParse; SubParse; ProdParse]


    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    let NegParse = pchar '-' >>. TermParse <?> "Neg" |>> fun a -> Mul(N (-1), a)
    let PointParse = pPointValue >*>. AtomParse |>> PV <?> "PV"
    let NParse   = pint32 |>> N <?> "Int"
    let VParse   = pid |>> V <?> "String"
    let charToIntParse = pCharToInt >*>. parenthesise CexpParse |>> CharToInt

    let ParParse = parenthesise TermParse
    do aref := choice [charToIntParse; NegParse; NParse; ParParse; PointParse; VParse ]

    
    let CParse   = psinglequotes palphanumeric <|> psinglequotes whitespaceChar |>> C <?> "Char"

    let toUpperParse = pToUpper >*>. parenthesise CexpParse |>> ToUpper <?> "toUpper" 
    let toLowerParse = pToLower >*>. parenthesise CexpParse |>> ToLower <?> "toLower" 
    let charValueParse = pCharValue >*>. parenthesise AexpParse |>> CV <?> "charValue"
    let intToCharParse = pIntToChar >*>. parenthesise AexpParse |>> IntToChar <?> "IntToChar"
    
    do cref := choice [toUpperParse; toLowerParse; intToCharParse; charValueParse; CParse]

    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }

    let BTermParse, btref = createParserForwardedToRef<bExp>()
    let BProdParse, bpref = createParserForwardedToRef<bExp>()
    let BAtomParse, baref = createParserForwardedToRef<bExp>()

    let BexpParse = BTermParse

    let AndParse = binop (pstring "/\\") BProdParse BTermParse <?> "And" |>> fun (a, b) -> a .&&. b
    let OrParse = binop (pstring "\\/") BProdParse BTermParse <?> "Or" |>> fun (a, b) -> a .||. b
    btref := choice [AndParse; OrParse; BProdParse]

    let EqParse = binop (pchar '=') AexpParse AexpParse <?> "Eq" |>> fun (a, b) -> a .=. b
    let NotEqParse = binop (pstring "<>") AexpParse AexpParse <?> "Not Eq" |>> fun (a, b) -> a .<>. b
    let LessThanParse = binop (pchar '<') AexpParse AexpParse <?> "Less than" |>> fun (a, b) -> a .<. b
    let LessThanOrEqParse = binop (pstring "<=") AexpParse AexpParse <?> "Less than or equal" |>> fun (a, b) -> a .<=. b
    let GreaterThanParse = binop (pchar '>') AexpParse AexpParse <?> "Greater than" |>> fun (a, b) -> a .>. b
    let GreaterThanOrEqParse = binop (pstring ">=") AexpParse AexpParse <?> "Greater than or equal" |>> fun (a, b) -> a .>=. b
    bpref := choice [EqParse; NotEqParse; LessThanParse; LessThanOrEqParse; GreaterThanParse; GreaterThanOrEqParse; BAtomParse]

    let BNegParse = pchar '~' >*>. BTermParse <?> "Neq" |>> Not
    let IsLetterParse = pIsLetter >*>. parenthesise CexpParse <?> "IsLetter" |>> IsLetter
    let IsVowelParse = pIsVowel >*>. parenthesise CexpParse <?> "IsVowel" |>> IsVowel
    let IsDigitParse = pIsDigit >*>. parenthesise CexpParse <?> "IsDigit" |>> IsDigit
    let BTrueParse = pTrue <?> "True" |>> fun _ -> TT
    let BFalseParse = pFalse <?> "False" |>> fun _ -> FF
    let BParParse = parenthesise BexpParse
    baref := choice [BNegParse; IsLetterParse; IsVowelParse; IsDigitParse; BTrueParse; BFalseParse; BParParse]


    let stmParse, stmref = createParserForwardedToRef<stm>()
    let stmAtomParse, stmAtomref = createParserForwardedToRef<stm>()

    let stmntParse = stmParse

    let declareParse = pdeclare >>. spaces1 >>. pid |>> Declare <?> "Declare"
    let AssParse = binop (pstring ":=") pid AexpParse |>> Ass <?> "Ass"
    let SeperatorParse = binop (pchar ';') stmAtomParse stmntParse |>> Seq <?> "Semicolon"
    let IfThenElseParse = pif >*>. parenthesise BexpParse .>*> pthen .>*>. pcurlybrackets stmntParse .>*> pelse .>*>. pcurlybrackets stmntParse |>> fun ((a, b), c) -> ITE(a,b,c)
    let IfThenParse = pif >*>. parenthesise BexpParse .>*> pthen .>*>. pcurlybrackets stmntParse |>> fun (a, b) -> ITE(a,b, Skip)
    let WhileParse = pwhile >*>. parenthesise BexpParse .>*> pdo .>*>. pcurlybrackets stmntParse |>>  While

    stmref := choice [ SeperatorParse; stmAtomParse ]
    stmAtomref := choice [ declareParse; AssParse; WhileParse; IfThenElseParse; IfThenParse ]

    let parseSquareProg (sqp: squareProg) = Map.map (fun k t -> run stmntParse t |> getSuccess |> stmntToSquareFun) sqp
    
    let parseBoardProg (s: string) (sqs: Map<int, square>) : boardFun2 =
        // printfn "%A" s 
        let statement = (run stmntParse s |> getSuccess)
        stmntToBoardFun statement sqs


    let mkBoard (bp : boardProg) : board = 
        let a: Map<int,square> = Map.map (fun k v -> parseSquareProg v) bp.squares
        { center = bp.center; defaultSquare = Map.find bp.usedSquare a; squares = parseBoardProg bp.prog a }

