// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval

    open StateMonad

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsConsonant of cExp  (* check for constant *)
       | IsLetter of cExp
       | IsDigit of cExp

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let binop f a b =
        a >>= fun x -> 
        b >>= fun y ->
        ret (f x y)

    let div a b = 
        a >>= fun x -> 
        b >>= fun y -> 
            if y = 0 then fail DivisionByZero
            else ret  (x / y)
    
    let modulo a b = 
        a >>= fun x -> 
        b >>= fun y -> 
            if y = 0 then fail DivisionByZero
            else ret  (x % y)

    let rec arithEval a : SM<int> = 
        match a with
        | N ex -> ret ex
        | V key -> lookup key
        | WL -> wordLength
        | PV i -> arithEval i >>= fun a -> pointValue a
        | Add (a, b) -> binop ( + ) (arithEval a) (arithEval b)
        | Sub (a, b) -> binop ( - ) (arithEval a) (arithEval b)
        | Mul (a, b) -> binop ( * ) (arithEval a) (arithEval b)
        | Div (a, b) -> div (arithEval a) (arithEval b)
        | Mod (a, b) -> modulo (arithEval a) (arithEval b)
        | CharToInt a -> charEval a >>= fun x -> ret (int x)


    and charEval c : SM<char> =
        match c with
        | C ex -> ret ex
        | CV ex -> arithEval ex >>= fun x -> characterValue x
        | ToUpper ex -> charEval ex >>= fun x -> ret (System.Char.ToUpper x)
        | ToLower ex -> charEval ex >>= fun x -> ret (System.Char.ToLower x)
        | IntToChar a -> arithEval a >>= fun x -> ret (char x)

    and boolEval b : SM<bool> = 
        match b with
        | TT -> ret true
        | FF -> ret false

        | AEq (a, b) -> ret ((arithEval a) = (arithEval b))
        | ALt (a, b) -> binop ( < ) (arithEval a) (arithEval b)

        | Not a -> boolEval a >>= fun x -> ret (not x)
        | Conj  (a, b) -> binop ( && ) (boolEval a) (boolEval b)

        | IsVowel a -> charEval a >>= fun x -> ret ("aeuioAEOIU".Contains x)
        | IsConsonant a -> charEval a >>= fun x -> ret ("bcdfghjklmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ".Contains x)
        | IsLetter a -> charEval a >>= fun x -> ret (System.Char.IsLetter x)
        | IsDigit a -> charEval a >>= fun x -> ret (System.Char.IsDigit x) 


    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun stm = failwith "Not implemented"


    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun stm m = failwith "Not implemented"

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"