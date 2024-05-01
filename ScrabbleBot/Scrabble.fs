namespace CoolestBotAround

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

// TODO: The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localized in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player number, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32> // Multiset of piece ids and their counts, received from the server
        playerTurn    : uint32
        numPlayers    : uint32
        playersAlive  : uint32 list
        tiles         : Map<uint32, tile>
    }

    let mkState b d pn h playerTurn numPlayers  t = {
        board = b; 
        dict = d;  
        playerNumber = pn; 
        hand = h; 
        playerTurn = playerTurn; 
        numPlayers = numPlayers;
        playersAlive = [1u..numPlayers] 
        tiles = t;
    }

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand

module Scrabble =
    open System.Threading
    //list with legal moves
    let legalMoves = []
    //best move so far should be a index in legalMoves
    let bestMove = 0
    
    //legelmove should caclulate the score of the move. First check if it is the best move so far and if so, save it to bestmove and then add it to the list of legal moves
    let legalMove (word) =
        //calculate the score of the move and add it to the list of legal moves then check if it is the best move so far and if so, save it to bestmove
        failwith "Not implemented"

    let updatePlayerTurn (st : State.state) = 
        //clear the legalmoves list when player turn changes
        let rec aux (l : uint32 list) = 
            match l with
            | [] -> failwith "No players left"; failwith "No players left"
            | h::t when h=st.playerTurn -> 
                match t with 
                | [] -> List.head st.playersAlive
                | _ -> List.head t
            | _::t -> aux t
        aux st.playersAlive
        
    let playerForfeit (st : State.state) =
        //remove player from playersAlive
        let playersAlive' = List.filter (fun x -> x <> st.playerNumber) st.playersAlive
        {st with playersAlive = playersAlive'}

    let handToTiles (hand : MultiSet.MultiSet<uint32>) (tiles : Map<uint32, tile>): List<tile> =
        MultiSet.toList hand |> List.map (fun x -> Map.find x tiles)

    type word = (char * int) list

    //get the move to play
    let rec getMove (st : State.state) =
        let tilesWithWildcars = handToTiles st.hand st.tiles

        // Ignore wildcards
        let tiles = tilesWithWildcars |> List.map (fun x -> Set.toSeq x |> Seq.head) 

        let applyTile (tl: char*int) (dict: Dictionary.Dict) currentWord : (Dictionary.Dict * bool) option =
            let cur = Dictionary.step (fst tl) dict
            match cur with
            | Some (_, d) -> 
                let test = Dictionary.step '#' d
                match test with
                | Some (true, _) -> 
                    Some (d, true)
                | _ -> Some (d, false)
            | None -> None
        
        let rec applyTileList (tiles: (char*int) list) (d: Dictionary.Dict) (currentWord: word) (acc: word Set) : word Set  =
            // Todo: stop passing word, to all recursive calls
            let res = List.map (fun x -> 
                let newList = List.filter (fun y -> y <> x) tiles
                if List.isEmpty newList then acc
                else
                    let d' = applyTile x d currentWord
                    match d' with
                    | Some (d', true) -> 
                        applyTileList newList d' (currentWord@[x]) (Set.add (currentWord@[x]) acc);
                    | Some (d', false) ->
                        applyTileList newList d' (currentWord@[x]) acc;
                    | None -> acc
                )  
            
            Set.unionMany (res tiles)
                   
        let help = applyTileList tiles st.dict [] Set.empty
        let words: word list = Set.toList help
        let reverse: word list = List.map (fun x -> List.rev x) words
        let ordered: ((word list) * int) = (reverse, 0)
        forcePrint (sprintf "Words: %A\n" reverse)
        SMPass

    let UpdateBoard (st : State.state) (move : list<coord * (uint32 * (char * int))>) =
        failwith "Not implemented"


    let playGame cstream pieces (st : State.state) =
        let rec aux (st : State.state) =
            if(st.playerTurn = st.playerNumber) then
                // Print.printHand pieces (State.hand st)
                // remove the force print when you move on from manual input (or when you have learnt the format)
                let move = getMove st
                //let move = SMPass
                let test = Dictionary.step 'S' st.dict  
                match test with
                | Some (_, dict) ->
                    forcePrint (sprintf "START")
                    let test = Dictionary.step 'W' dict
                    match test with
                    | Some (_, dict) -> 
                        let test = Dictionary.step 'O' dict
                        match test with
                        | Some (_, dict) -> 
                            let test = Dictionary.step 'R' dict
                            match test with
                            | Some (_, dict) ->
                                let test = Dictionary.step 'G' dict
                                match test with
                                | Some (_, dict) ->  
                                    let test = Dictionary.step '#' dict
                                    match test with
                                    | Some (isWord, dict) -> 
                                        forcePrint (sprintf "IS WORD %b" isWord)
                                        forcePrint (sprintf "IS WORD %b" isWord)
                                        forcePrint (sprintf "IS WORD %b" isWord)
                                        forcePrint (sprintf "IS WORD %b" isWord)
                                        forcePrint (sprintf "IS WORD %b" isWord)
                        "a"
                |> ignore
                
                
                let move = SMPass  
                forcePrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                //send cstream (move)

            let msg = recv cstream
            forcePrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) msg) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(move, points, newTiles)) ->
                forcePrint (sprintf "Successful play! Points: %d\n" points)   
                Printf.printf "Successful play! Points: %d\n" points
                let nextPlayer = updatePlayerTurn st
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let tilesToBeAddedToHand = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty newTiles
                let tilesUsed = List.fold (fun acc (_, (tileId, (_, _))) -> MultiSet.add tileId 1u acc) MultiSet.empty move
                let hand' = MultiSet.subtract (State.hand st) tilesUsed
                let hand'' = MultiSet.union hand' tilesToBeAddedToHand
                //TODO: update the board
                let st' = {st with hand = hand'' ; playerTurn = nextPlayer}
                aux st'
                
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                forcePrint (sprintf "Player %d played a word! Points: %d\n" pid points)
                let nextPlayer = updatePlayerTurn st
                forcePrint (sprintf "NEW TURN IS: %d\n" nextPlayer)

                let st' = {st with playerTurn = nextPlayer} // This state needs to be updated
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                forcePrint (sprintf "Player %d failed to play a word!\n" pid)
                (* Failed play. Update your state *)
                // This is only for your plays
                // TODO maybe do something else than parse
                //send cstream SMPass
                //let st' = st // This state needs to be updated
                aux st
            | RCM (CMPassed (pid)) -> 
                // Passed. Update your state
                forcePrint (sprintf "Player %d passed!\n" pid)
                let nextPlayer = updatePlayerTurn st
                let st' = {st with playerTurn = nextPlayer} // This state needs to be updated
                aux st'
            | RCM (CMForfeit (pid)) -> 
                forcePrint (sprintf "Player %d forfeited!\n" pid)
                // Forfeit. Update your state
                let st' = playerForfeit st // This state needs to be updated
                aux st'
            // Other player changed there hand
            | RCM (CMChange (playerId, numberOfTiles)) ->
                let nextPlayer = updatePlayerTurn st
                let st' = {st with playerTurn = nextPlayer} // This state needs to be updated
                aux st'
            | RCM (CMTimeout(playerId) ) ->
                // Player timed out could be me
                let nextPlayer = updatePlayerTurn st
                let st' = {st with playerTurn = nextPlayer} // This state needs to be updated
                aux st'
            
            | RCM (CMGameOver _) -> forcePrint (sprintf "Game Over\n")
            | RCM a ->
                    forcePrint (sprintf "FAILURE!! not implemented: %A\n" a)
                    failwith (sprintf "not implemented: %A" a)
                    aux st
            | RGPE err -> forcePrint (sprintf "Gameplay Error:\n%A" err); aux st
        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //TODO: use a trie or gaddag
        let dict = dictf true // Uncomment if using a gaddag for your dictionary
        //let dict = dictf false // Uncomment if using a trie for your dictionary
            
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet playerTurn numPlayers tiles )
        