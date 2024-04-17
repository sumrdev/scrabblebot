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
    let pid = pletter <|> pchar '_' .>>. many (pletter <|> palphanumeric) |>> fun ((a, b)) -> a::b |> charListToString

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

    let BexpParse = pstring "not implemented"

    let stmParse = pstring "not implemented"

    (* The rest of your parser goes here *)
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}
