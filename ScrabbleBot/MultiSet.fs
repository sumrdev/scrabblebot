module internal MultiSet

    type MultiSet<'a when 'a : comparison> = R of Map<'a, uint32> // replace with your type

    let empty = R Map.empty

    let isEmpty (R (s):MultiSet<'a>) = Map.isEmpty s

    let size (R (s): MultiSet<'a>) = s |> Map.values |> Seq.sum
    
    let contains (key : 'a) (R (s) : MultiSet<'a>) = Map.containsKey key s

    let numItems (key : 'a) (R (s) : MultiSet<'a>) = 
        match s.ContainsKey key with
        | true -> s[key]
        | _ -> 0u

    let add (key : 'a) (value : uint32) (R (s)  : MultiSet<'a>) = 
        match value with
        | 0u -> s |> R
        | _ -> Map.add key ((numItems key (R s)) + value) s |> R
    
    let set (key : 'a) (value : uint32) (R (s)  : MultiSet<'a>) = 
        Map.add key value s |> R
    
    let addSingle (key : 'a) s  = add key 1u s
    
    let remove (key : 'a) (value : uint32) (R(s)  : MultiSet<'a>) = 
        let newVal = int ((numItems key (R s))-value)
        match newVal with
        | newVal when newVal > 0 -> Map.add key (uint newVal) s |> R
        | _ -> Map.remove key s |> R
        
    let removeSingle (key : 'a) (s : MultiSet<'a>) : MultiSet<'a>= remove key 1u s

    let fold (folder : 'b -> 'a -> uint32 -> 'b) (acc : 'b) (R(s) : MultiSet<'a>) = Map.fold folder acc s

    let foldBack (folder : 'a -> uint32 -> 'b -> 'b) (R(s: Map<'a,uint32>)) (acc : 'b) = Map.foldBack folder s acc
    
    let ofList (list : 'a list) : MultiSet<'a> = List.fold (fun acc x -> addSingle  x acc) empty list
    let toList (R(s: Map<'a,uint32>) : MultiSet<'a>) : 'a list = 
        Map.fold (fun acc k v -> acc @ List.replicate (int v) k) [] s

    let map (mapper : 'a -> 'b) (R(s: Map<'a,uint32>) : MultiSet<'a>) : MultiSet<'b> = 
        Map.fold (fun acc k v -> (add (mapper k) v) acc) empty s

    let union s1 s2 : MultiSet<'b> = 
        fold (fun acc k v -> acc |> (numItems k acc |> max v |> set k)) s1 s2
    let sum (R(s1: Map<'a,uint32>) : MultiSet<'a>) (s2 : MultiSet<'a>) : MultiSet<'a> =  
        Map.map (fun k v -> (numItems k s2)+ v ) s1 |> R
    let subtract s1 s2 : MultiSet<'a> = 
        fold (fun acc k v ->  remove k v acc) s1 s2
    
    let intersection (s1: MultiSet<'a>) (s2 : MultiSet<'a>) : MultiSet<'a> =  
        subtract s1 s2 |> subtract s1