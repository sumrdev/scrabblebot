module internal Dict
    type Dict = 
    | Node of bool * Map<char, Dict>

    let empty = Node(false, Map.empty)

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
                    | None -> empty
                // Add the rest of the word to the child recursively
                let newChild = add' cs child
                // Add the new child to the children
                Node(isWord, children.Add(c, newChild))
        add' (word |> Seq.toList ) dict

    let lookup (word: string) (dict: Dict) =
        let rec contains' (word: char list) (dict: Dict) = 
            match word, dict with
            // If no more chars - return isWord
            | [], Node(isWord, _) -> isWord
            // If there are chars left - check if the first char is in the children
            | c::cs, Node(_, children) -> 
                match Map.tryFind c children with
                | Some t -> contains' cs t
                | None -> false
        contains' (word |> Seq.toList) dict

    let is_prefix (word: string) (dict: Dict) = 
        let rec prefix' (word: char list) (dict: Dict) = 
            match word, dict with
            // If no more chars - return true
            | [], _ -> true
            // If there are chars left - check if the first char is in the children
            | c::cs, Node(_, children) -> 
                match Map.tryFind c children with
                | Some t -> prefix' cs t
                | None -> false
        prefix' (word |> Seq.toList) dict

    let prefix (word: string) (dict: Dict) : Dict =
        let rec prefix' (word: char list) (dict: Dict) = 
            match word, dict with
            // If no more chars - return the dict
            | [], _ -> dict
            // If there are chars left - check if the first char is in the children
            | c::cs, Node(_, children) -> 
                match Map.tryFind c children with
                | Some t -> prefix' cs t
                | None -> empty
        prefix' (word |> Seq.toList) dict

    // step returns a bool indicating if the current node is a word and the next node
    let step (c: char) (dict: Dict) : bool * Dict =
        // remember that the relevant boolean is in the child
        match dict with
        | Node(isWord, children) -> 
            match Map.tryFind c children with
            | Some t -> 
                match t with
                | Node(isWord, _) -> isWord, t
            | None -> false, empty