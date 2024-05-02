module internal ScrabbleBot
    open ScrabbleUtil
    open ScrabbleUtil.DebugPrint

    
    //TODO: should check the game board for holes and border and other things
    (* let checkVaildPosition = 
        failwith "Not implemented"  *)
    type unplacedTile = uint32 * (char * int)
    type tileInstance = coord * unplacedTile
    let mutable finalWords: list<list<tileInstance>> = []

    type genState = {
        position: int
        word: tileInstance list
        rack: MultiSet.MultiSet<uint32>
        playableRack: MultiSet.MultiSet<uint32>
        gaddag: Dictionary.Dict
        anchor: coord
        playedTiles: Map<coord, tileInstance>
        vertical: bool
        tiles: Map<uint32, tile>
        tilesAdded: int
        
        //Timeout stuff
    }
    
    let mkGenState (position: int) (word: tileInstance list)(rack: MultiSet.MultiSet<uint32>) (playableRack: MultiSet.MultiSet<uint32>) (gaddag: Dictionary.Dict) (anchor: coord) (playedTiles: Map<coord, tileInstance>) (vertical: bool) (tiles: Map<uint32, tile>) (tilesAdded: int) = {
        position = position; 
        word = word; 
        rack = rack; 
        playableRack = playableRack;
        gaddag = gaddag; 
        anchor = anchor; 
        playedTiles = playedTiles; 
        vertical = vertical; 
        tiles = tiles;
        tilesAdded=tilesAdded; 
        }
    
    let recordPlay(word: tileInstance list) =
        let letters =  List.map (fun (coord, (id, (c, _))) -> c) word
        let reversed = List.rev letters
        let string = new string (reversed |> List.toArray)
        //forcePrint (sprintf "Found valid word: %A\n"  string)
        finalWords <- finalWords @ [word]

    let getcoord (s: genState) = 
        match s.vertical with
        | true -> 
            let (x, y) = s.anchor
            (x, y + s.position)
        | false -> 
            let (x, y) = s.anchor
            (x + s.position, y)

    let getChar (c: unplacedTile) = 
        let (_, (c, _)) = c
        c
    
    let getCharTile (c: tileInstance) = 
        let (_, (_, (c, _))) = c
        c

    let lookup (s: genState): tileInstance option =
        let newCoord: coord = getcoord s
        match s.playedTiles.TryGetValue newCoord with
        | true, c -> Some c
        | _ -> None
        
    let removeFromRack (c: uint32) (rack: MultiSet.MultiSet<uint32>) = 
        MultiSet.remove c 1u rack
    
    let getTileList (id: uint32) (tiles: Map<uint32, tile>) : unplacedTile list = 
        match Map.tryFind id tiles with
        | Some(t) -> List.map (fun t -> (id, t)) (Set.toList t)
        | _ -> [] 
    

    let checkValidPlay (s: genState): unit =
        if s.tilesAdded = 0 then
            ()
        else
        match Dictionary.step '#' s.gaddag with
            | Some (true, _) -> 
                match lookup s with
                | Some l -> ()
                | None -> recordPlay s.word // record if no letter to the left
            | _ -> ()


    let validPerpendicularLetters (s: genState) : MultiSet.MultiSet<uint32> =

        s.rack        

    let gen (s: genState) = 
        finalWords <- []
        let rec gen' (s: genState) =
            match lookup s with
            | Some l -> 
                let (_, unplaced) = l
                let newGaddag = Dictionary.step (getCharTile l) s.gaddag
                match newGaddag with
                | Some (_, gaddag) -> GoOn unplaced false {s with gaddag=gaddag }
                | None -> ()
            | None   -> 
                let rackList = MultiSet.toList s.rack
                match rackList |> List.isEmpty with
                | false ->  
                    // for each letter on the rack and in the next step of gaddag call GoOn 
                    List.map (fun id -> 
                        let tileList = getTileList id s.tiles
                        List.map (fun (u: unplacedTile) ->
                            let newRack = (removeFromRack id s.rack)
                            let newGaddag = Dictionary.step (getChar u) s.gaddag
                            match newGaddag with
                            | Some (_, gaddag) -> GoOn u true {s with rack=newRack; tilesAdded=(s.tilesAdded + 1); gaddag=gaddag }
                            | None -> ()
                        ) tileList
                    ) rackList |> ignore
                    ()
                | _ -> () // no letters on the rack
    

        and GoOn (letter: unplacedTile) (ourTile: bool) (s: genState) =
            let testAnchor1 = 
                match s.vertical with
                | true  -> (fst s.anchor - 1, snd s.anchor)
                | false -> (fst s.anchor, snd s.anchor - 1)
            let testAnchor2 =
                match s.vertical with
                | true  -> (fst s.anchor + 1, snd s.anchor)
                | false -> (fst s.anchor , snd s.anchor + 1)
            let leftSqr = lookup {s with anchor = testAnchor1}
            let rightSqr = lookup {s with anchor = testAnchor2}
            match leftSqr, rightSqr, ourTile with
            | Some _, _, true -> ()
            | _, Some _, true -> ()
            | _ ->
                match s.position <= 0 with
                | true -> 
                
                    let newCoord = getcoord s
                    let newWord =  
                        match ourTile with
                        true -> s.word @ [(newCoord, letter)]
                        | false -> s.word

                    checkValidPlay {s with word=newWord; position=(s.position-1)} |> ignore
                    gen' { s with position=(s.position - 1); word=newWord }// only if space to the left, but dont matter for now
                    let gaddag' = Dictionary.step '#' s.gaddag
                    match gaddag' with
                    | Some (_, gaddag') ->
                        gen' {s with position=(1); word=newWord; gaddag=gaddag' }
                    | None -> ()      
                    
                    // if letter is on old arc no letter to the left call recordPlay
                | false -> 
                    let newCoord = getcoord s
                    let newWord = [(newCoord, letter)] @ s.word 
                    checkValidPlay {s with word=newWord; position=(s.position+1)} |> ignore
                    gen' { s with position=(s.position + 1); word=newWord}

        gen' s
        finalWords
        