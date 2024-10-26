﻿// Insert your StateMonad.fs from Assignment 6 here. All modules must be internal.


module internal StateMonad

    type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero 
        | ReservedName of string
        | ArgumentException of string      

    type Result<'a, 'b>  =
        | Success of 'a
        | Failure of 'b

    type State = { vars     : Map<string, int> list
                   word     : (char * int) list 
                   reserved : Set<string> }

    type SM<'a> = S of (State -> Result<'a * State, Error>)

    let mkState lst word reserved = 
           { vars = [Map.ofList lst];
             word = word;
             reserved = Set.ofList reserved }

    let evalSM (s : State) (S a : SM<'a>) : Result<'a, Error> =
        match a s with
        | Success (result, _) -> Success result
        | Failure error -> Failure error

    let bind (f : 'a -> SM<'b>) (S a : SM<'a>) : SM<'b> =
        S (fun s ->
              match a s with
              | Success (b, s') -> 
                match f b with 
                | S g -> g s'
              | Failure err     -> Failure err)


    let ret (v : 'a) : SM<'a> = S (fun s -> Success (v, s))
    let fail err     : SM<'a> = S (fun s -> Failure err)

    let (>>=)  x f = bind f x
    let (>>>=) x f = x >>= (fun () -> f)

    let push : SM<unit> = 
        S (fun s -> Success ((), {s with vars = Map.empty :: s.vars}))
    
    let pop : SM<unit> = S (fun s -> 
        match s.vars with
        | [] -> Failure(ArgumentException "No more states to pop")
        | _ :: rest -> Success((),{ s with vars = rest}))
    
    let wordLength : SM<int> = S (fun s -> Success (s.word.Length, s))   

    let characterValue (pos : int) : SM<char> = S (fun s -> 
       match pos with
        | i when i < 0 || i > s.word.Length-1 -> Failure(IndexOutOfBounds i)
        | _ -> Success(fst s.word.[pos], s)
    )

    let pointValue (pos : int) : SM<int> = S (fun s -> 
       match pos with
        | i when i < 0 || i > s.word.Length -> Failure(IndexOutOfBounds i)
        | _ -> Success(snd s.word.[pos], s)
    )     
    let lookup (x : string) : SM<int> = 
        let rec aux =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind x m with
                | Some v -> Some v
                | None   -> aux ms

        S (fun s -> 
              match aux (s.vars) with
              | Some v -> Success (v, s)
              | None   -> Failure (VarNotFound x))
    //TODO: see if this is needed
    let declare (var : string) : SM<unit> = 
        S (fun s -> 
            if Set.contains var s.reserved then Failure (ReservedName var)
            else 
                match s.vars with
                | [] -> Failure (ArgumentException "No states to declare in")
                | m :: ms -> 
                    match Map.tryFind var m with
                    | Some _ -> Failure (VarExists var)
                    | None   -> Success ((), { s with vars = (Map.add var 0 m) :: ms }))
    let update (var : string) (value : int) : SM<unit> = 
        S (fun s -> 
            let rec aux =
                function
                | []      -> None
                | m :: ms -> 
                    match Map.tryFind var m with
                    | Some _ -> Some (Map.add var value m, ms)
                    | None   -> 
                        match aux ms with
                        | Some (m', ms') -> Some (m, m' :: ms')
                        | None           -> None

            match aux s.vars with
            | Some (m, ms) -> Success ((), { s with vars = m :: ms })
            | None         -> Failure (VarNotFound var))