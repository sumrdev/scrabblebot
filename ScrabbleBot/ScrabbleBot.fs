module internal ScrabbleBot
    open ScrabbleUtil
    
    //mudible (coord * (uint32 * char)) list
    
    //should check the game board for holes and border and other things
    (* let checkVaildPosition = 
        failwith "Not implemented"  *)
    type tileInstance = coord * (uint32 * (char * int))
    
    let recordPlay(word, anchor, vertical) =
        failwith "Not implemented"
    let lookup (position: int) (anchor: coord) (playedTiles: Map<coord, tileInstance>) (vertical: bool): tileInstance option =
        match vertical with
        | true -> 
            let (x, y) = anchor
            let newCoord: coord = (x, y + position)
            match playedTiles.TryGetValue newCoord with
            | true, c -> Some c
            | _ -> None
        | false ->
            let (x, y) = anchor
            let newCoord: coord = (x + position, y)
            match playedTiles.TryGetValue newCoord with
            | true, c -> Some c
            | _ -> None
        
    let removeFromRack (c: uint32) (rack: MultiSet.MultiSet<uint32>) = 
        MultiSet.remove c 1u rack
    
    let getCharList (id: uint32) (tiles: Map<uint32, tile>) = 
        match Map.tryFind id tiles with
        | Some(t) -> List.map (fun t -> fst t) (Set.toList t)
        | _ -> [] 
    
    let checkValidPlay (newPos: int) gaddag anchor playedTiles vertical newWord =
        match Dictionary.step '#' gaddag with
            | Some (true, gaddag') -> 
                match lookup newPos anchor playedTiles vertical with
                | Some l -> 
                    //recordPlay
                    true
                | None -> false
            | _ -> false

    let rec gen (position: int) (word: tileInstance list) (rack: MultiSet.MultiSet<uint32>) gaddag (anchor: coord) (playedTiles: Map<coord, tileInstance>) (vertical: bool) (tiles : Map<uint32, tile>) = 
        let rec gen' (position: int) (word: tileInstance list) (rack: MultiSet.MultiSet<uint32>) gaddag =
            match lookup position anchor playedTiles vertical with
            | Some l -> 
                GoOn position l word rack (Dictionary.step l gaddag) gaddag anchor playedTiles vertical tiles
            | None   -> 
                let rackList = MultiSet.toList rack
                match rackList |> List.isEmpty with
                | true ->  
                    // for each letter on the rack and in the next step of gaddag call GoOn 
                    List.map (fun id -> 
                        let chars = getCharList id tiles
                        List.map (fun (c: tileInstance ->
                            GoOn position c word (removeFromRack id rack) (Dictionary.step c gaddag) gaddag anchor playedTiles vertical tiles
                        ) chars
                    ) rackList |> ignore
                    ()
                | _ -> () // no letters on the rack
        gen position word rack gaddag anchor playedTiles vertical tiles
    

    and GoOn (position: int) (letter: tileInstance) (word: tileInstance list) (rack: MultiSet.MultiSet<uint32>) (newGaddag: (bool * Dictionary.Dict) option) (oldGaddag: Dictionary.Dict) (anchor: coord) (playedTiles: Map<coord,tileInstance>) (vertical: bool) (tiles : Map<uint32, tile>) =
        match position > 0 with
        | true -> 
            let newWord =  word @ [letter]
            match newGaddag with
            | Some (_, gaddag) -> 
                match checkValidPlay (position-1) gaddag anchor playedTiles vertical newWord with
                | true -> 
                    gen (position + 1) newWord rack gaddag anchor playedTiles vertical tiles
                | false -> 
                    let gaddag' = Dictionary.step '#' oldGaddag
                    match gaddag' with
                    | Some (_, gaddag') ->
                        gen (position - 1) newWord rack gaddag' anchor playedTiles vertical tiles    
                    | None -> ()      
            | None -> ()       
            
            // if letter is on old arc no letter to the left call recordPlay
        | false -> 
            let newWord = [letter] @ word 
            match newGaddag with
            | Some (_, gaddag) -> 
                checkValidPlay (position+1) gaddag anchor playedTiles vertical |> ignore
                gen (position + 1) newWord rack gaddag anchor playedTiles vertical tiles
            | None -> ()