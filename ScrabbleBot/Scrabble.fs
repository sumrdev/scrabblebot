namespace CoolestBotAround

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint
open System.Threading

type tileInstance = coord * (uint32 * (char * int))

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
        dict          : Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32> // Multiset of piece ids and their counts, received from the server
        playerTurn    : uint32
        numPlayers    : uint32
        playersAlive  : uint32 list
        tiles         : Map<uint32, tile>
        round         : uint32
        playedTiles   : Map<coord,tileInstance>
        timeout       : uint32 option
    }

    let mkState b d pn h playerTurn numPlayers t timeout = {
        board = b; 
        dict = d;  
        playerNumber = pn; 
        hand = h; 
        playerTurn = playerTurn; 
        numPlayers = numPlayers;
        playersAlive = [1u..numPlayers] 
        tiles = t;
        round = 0u;
        playedTiles = Map.empty
        timeout = timeout
    }

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand

module Scrabble =
    open System.Threading.Tasks
    let updatePlayerTurn (st : State.state) = 
        //clear the legalmoves list when player turn changes
        let rec aux (l : uint32 list) = 
            match l with
            | [] -> aux st.playersAlive
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

    type word = (uint32 * (char * int)) list

    let getMailbox () = MailboxProcessor.Start(fun inbox ->
        let mutable finalWords: list<list<tileInstance>> = []
        let rec loop () = async {
            let! msg = inbox.Receive ()
            match msg with
            | ScrabbleBot.Add m -> finalWords <- finalWords @ [m]
            | ScrabbleBot.Get ch -> ch.Reply finalWords
            | ScrabbleBot.Clear () -> finalWords <- []
            return! loop () }
        loop ())

    //async function to get the possible moves
    let asyncGenMoves (st : State.state) (pos: coord) vertical mailbox = async {
        let state = ScrabbleBot.mkGenState 0 [] st.hand st.hand st.dict pos st.playedTiles vertical st.tiles 0 st.board mailbox
        return ScrabbleBot.gen state
    }

    let genInitalMoves (st : State.state) mailbox : unit =
        match st.playedTiles |> Map.isEmpty with
        | true ->
            asyncGenMoves st (0, 0) true mailbox |> Async.RunSynchronously
            asyncGenMoves st (0, 0) false mailbox |> Async.RunSynchronously
        | false -> ()

    let asyncGetMove (st : State.state) mailbox = async {

        let toCheckVertical = Map.fold (fun acc (x, y) v -> if  Map.containsKey (x, y+1) st.playedTiles then acc else acc@[fst v]) [] st.playedTiles
        let toCheckHorizontal = Map.fold (fun acc (x, y) v -> if  Map.containsKey (x+1, y) st.playedTiles then acc else acc@[fst v]) [] st.playedTiles
        
        let verticalMovesTasks = toCheckVertical |> List.map (fun x -> asyncGenMoves st x true mailbox)
        let horizontalMovesTasks = toCheckHorizontal |> List.map (fun x -> asyncGenMoves st x false mailbox)
        
        List.concat [verticalMovesTasks; horizontalMovesTasks] |> Async.Parallel |> Async.RunSynchronously |> ignore
        genInitalMoves st mailbox |> ignore
    }

    let getMoveFromMailbox (mailbox: MailboxProcessor<ScrabbleBot.msg>)  =
        let getPoints (word: tileInstance list) : int = List.fold (fun acc (_, (_, (_, v))) -> acc + v) 0 word
        
        let res = mailbox.PostAndAsyncReply (fun ch -> ScrabbleBot.Get ch) |> Async.RunSynchronously
        let withPoints: (tileInstance list * int) list = List.map (fun x -> (x, getPoints x)) res
        let sorted: (tileInstance list * int) list = List.sortBy (fun (x, k) -> -k) withPoints
        match sorted with
        | [] -> SMPass
        | m -> SMPlay (List.head m |> fst)

    let UpdateBoard (st : State.state) (move : list<tileInstance>) =
        
        let playedTiles = st.playedTiles
        let playedTiles' = List.fold (fun acc m -> 
            let coord, _ = m
            Map.add coord m acc) playedTiles move
        {st with playedTiles = playedTiles'}


    let playGame cstream pieces (st : State.state) =
        let rec aux (st : State.state) =
            if(st.playerTurn = st.playerNumber) then
                // Print.printHand pieces (State.hand st)
                let mailbox = getMailbox ()
                let moveTask = asyncGetMove st mailbox
                let ok () = send cstream (getMoveFromMailbox mailbox)
                let ex e = send cstream SMPass
                let can e = send cstream (getMoveFromMailbox mailbox)
                let cts = new CancellationTokenSource()

                Async.StartWithContinuations(moveTask, ok, ex, can, cts.Token) |> ignore
                let t= Async.StartAsTask(moveTask)
                let timeout = Async.StartAsTask (async {
                    match st.timeout with
                    | None -> do! Async.Sleep 100_000 
                    | Some t -> 
                        let intValue : int = int t
                        do! Async.Sleep intValue 
                })
                Task.WaitAny (t, timeout ) |> ignore
                cts.Cancel()

            let msg = recv cstream
            //forcePrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) msg) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(move, points, newTiles)) ->
                //forcePrint (sprintf "Successful play! Points: %d\n" points)   
                //Printf.printf "Successful play! Points: %d\n" points
                let nextPlayer = updatePlayerTurn st
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let tilesToBeAddedToHand = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty newTiles
                let tilesUsed = List.fold (fun acc (_, (tileId, (_, _))) -> MultiSet.add tileId 1u acc) MultiSet.empty move
                let hand' = MultiSet.subtract (State.hand st) tilesUsed
                let hand'' = MultiSet.union hand' tilesToBeAddedToHand
                //TODO: update the board
                let st' = UpdateBoard st move
                let st'' = {st' with hand = hand'' ; playerTurn = nextPlayer}
                aux st''
                
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let nextPlayer = updatePlayerTurn st
                let st' = UpdateBoard st ms
                let st'' = {st' with playerTurn = nextPlayer} // This state needs to be updated
                aux st''
            | RCM (CMPlayFailed (pid, ms)) ->
                let nextPlayer = updatePlayerTurn st
                let st' = {st with playerTurn = nextPlayer} // This state needs to be updated
                aux st'
            | RCM (CMPassed (pid)) -> 
                forcePrint (sprintf "Player %d passed!\n" pid)
                let nextPlayer = updatePlayerTurn st
                let st' = {st with playerTurn = nextPlayer} // This state needs to be updated
                aux st'
            | RCM (CMForfeit (pid)) -> 
                let st' = playerForfeit st // This state needs to be updated
                aux st'
            | RCM (CMChange (playerId, numberOfTiles)) ->
                let nextPlayer = updatePlayerTurn st
                let st' = {st with playerTurn = nextPlayer} // This state needs to be updated
                aux st'
            | RCM (CMTimeout(playerId) ) ->
                let nextPlayer = updatePlayerTurn st
                let st' = {st with playerTurn = nextPlayer} // This state needs to be updated
                aux st'
            
            | RCM (CMGameOver _) -> forcePrint (sprintf "Game Over\n")
            | RCM a ->
                aux st
            | RGPE err -> 
                aux st
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

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet playerTurn numPlayers tiles timeout)
        