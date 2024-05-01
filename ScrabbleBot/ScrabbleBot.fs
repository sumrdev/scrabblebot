module internal ScrabbleBot
    open ScrabbleUtil
    open ScrabbleUtil.DebugPrint

    
    //mudible (coord * (uint32 * char)) list
    
    //should check the game board for holes and border and other things
    (* let checkVaildPosition = 
        failwith "Not implemented"  *)
    type unplacedTile = uint32 * (char * int)
    type tileInstance = coord * unplacedTile
    let mutable finalWords: list<list<tileInstance>> = []
    
    let recordPlay(word: tileInstance list) =
        let letters =  List.map (fun (coord, (id, (c, _))) -> c) word
        let reversed = List.rev letters
        let string = new string (reversed |> List.toArray)
        forcePrint (sprintf "Found valid word: %A\n"  string)
        finalWords <- finalWords @ [word]


    let getcoord (pos: int) (anchor: coord) (vertical: bool) = 
        match vertical with
        | true -> 
            let (x, y) = anchor
            (x, y + pos)
        | false -> 
            let (x, y) = anchor
            (x + pos, y)

    let getChar (c: unplacedTile) = 
        let (_, (c, _)) = c
        c
    
    let getCharTile (c: tileInstance) = 
        let (_, (_, (c, _))) = c
        c

    let lookup (position: int) (anchor: coord) (playedTiles: Map<coord, tileInstance>) (vertical: bool): tileInstance option =
        let newCoord: coord = getcoord position anchor vertical
        match playedTiles.TryGetValue newCoord with
        | true, c -> Some c
        | _ -> None
        
    let removeFromRack (c: uint32) (rack: MultiSet.MultiSet<uint32>) = 
        MultiSet.remove c 1u rack
    
    let getTileList (id: uint32) (tiles: Map<uint32, tile>) : unplacedTile list = 
        match Map.tryFind id tiles with
        | Some(t) -> List.map (fun t -> (id, t)) (Set.toList t)
        | _ -> [] 
    

    let checkValidPlay (newPos: int) gaddag anchor playedTiles vertical newWord =
        match Dictionary.step '#' gaddag with
            | Some (true, gaddag') -> 
                match lookup newPos anchor playedTiles vertical with
                | Some l -> true
                | None -> 
                    recordPlay newWord  // record if no letter to the left
                    false
            | _ -> false

    let gen (position: int) (word: tileInstance list) (rack: MultiSet.MultiSet<uint32>) gaddag (anchor: coord) (playedTiles: Map<coord, tileInstance>) (vertical: bool) (tiles : Map<uint32, tile>) = 
        let rec gen' (position: int) (word: tileInstance list) (rack: MultiSet.MultiSet<uint32>) gaddag =
            match lookup position anchor playedTiles vertical with
            | Some l -> 
                let (_, unplaced) = l
                GoOn position unplaced word rack (Dictionary.step (getCharTile l) gaddag) gaddag anchor playedTiles vertical
            | None   -> 
                let rackList = MultiSet.toList rack
                match rackList |> List.isEmpty with
                | false ->  
                    // for each letter on the rack and in the next step of gaddag call GoOn 
                    List.map (fun id -> 
                        let tileList = getTileList id tiles
                        List.map (fun (c: unplacedTile) ->
                            GoOn position c word (removeFromRack id rack) (Dictionary.step (getChar c) gaddag) gaddag anchor playedTiles vertical
                        ) tileList
                    ) rackList |> ignore
                    ()
                | _ -> () // no letters on the rack
    

        and GoOn (position: int) (letter: unplacedTile) (word: tileInstance list) (rack: MultiSet.MultiSet<uint32>) (newGaddag: (bool * Dictionary.Dict) option) (oldGaddag: Dictionary.Dict) (anchor: coord) (playedTiles: Map<coord,tileInstance>) (vertical: bool) =
            match position <= 0 with
            | true -> 
                let newCoord = getcoord position anchor vertical
                let newWord =  word @ [(newCoord, letter)]
                match newGaddag with
                | Some (_, gaddag) -> 
                    checkValidPlay (position-1) gaddag anchor playedTiles vertical newWord |> ignore
                    match true with // space to the left
                    | true -> 
                        gen' (position - 1) newWord rack gaddag
                    | false -> 
                        let gaddag' = Dictionary.step '#' gaddag
                        match gaddag' with
                        | Some (_, gaddag') ->
                            gen'(1) newWord rack gaddag'
                        | None -> ()      
                | None -> ()       
                
                // if letter is on old arc no letter to the left call recordPlay
            | false -> 
                let newCoord = getcoord position anchor vertical
                let newWord = [(newCoord, letter)] @ word 
                match newGaddag with
                | Some (_, gaddag) -> 
                    checkValidPlay (position+1) gaddag anchor playedTiles vertical newWord |> ignore
                    gen' (position + 1) newWord rack gaddag
                | None -> ()
            

        gen' position word rack gaddag
        finalWords
        