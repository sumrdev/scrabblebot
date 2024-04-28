module Dictionary
    type Dict = 
    | Node of bool * Map<char, Dict>

    let empty : unit -> Dict = fun () -> Node(false, Map.empty)

    let permutations (word: string) : char list list  =
        let list = [1 .. word.Length]
        let rec permute (word: char list) : char list list= 
            List.fold (
                fun acc i -> 
                    ((word.[i .. (word.Length)] |> List.rev )  @ ['#']  @ word.[0..i-1] |> List.rev ) :: acc)
                []
                list
        permute (word |> Seq.toList)

    let insert (word: string) (dict: Dict) = 
        let rec add' (word: char list) (dict: Dict) = 
            match word, dict with
            // If no more chars - replace the node with the same children but isWord = true
            | [], Node(_, children) -> Node(true, children)
            // If there are chars left - add the first char to the children
            | c::cs, Node(isWord, children) -> 
                let child = 
                    // If there is already a child with the same char - use it
                    match Map.tryFind c children with
                    | Some t -> t
                    | None -> empty ()
                // Add the rest of the word to the child recursively
                let newChild = add' cs child
                // Add the new child to the children
                Node(isWord, children.Add(c, newChild))
        //add the permutations of the word to the dictionary
        permutations word |> List.fold (fun acc p -> add' p acc) dict

    let step (c: char) (dict: Dict) : (bool * Dict) option =
        match dict with
        | Node(isWord, children) -> 
            match Map.tryFind c children with
            | Some t -> 
                match t with
                | Node(isWord, _) -> Some(isWord, t)
            | None -> None
    let reverse (dict: Dict) : (bool * Dict) option = 
        step '#' dict

    let lookup (word: string) (dict: Dict) =
        let rec lookup' (word: char list) (dict: Dict) = 
            match word, dict with
            | [], Node(isWord, _) -> isWord
            | c::cs, dict -> 
                match step c dict with
                | Some(_, next) -> lookup' cs next
                | None -> 
                    match reverse dict with
                    | Some(_, next) -> lookup' word next
                    | None -> false
        lookup' (word |> Seq.toList) dict
