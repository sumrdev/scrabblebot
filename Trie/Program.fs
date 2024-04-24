open System.IO
type Trie = 
    | Node of bool * Map<char, Trie>

let empty = Node(false, Map.empty)

let add (word: string) (trie: Trie) = 
    let rec add' (word: char list) (trie: Trie) = 
        match word, trie with
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
    add' (word |> Seq.toList ) trie

let contains (word: string) (trie: Trie) =
    let rec contains' (word: char list) (trie: Trie) = 
        match word, trie with
        // If no more chars - return isWord
        | [], Node(isWord, _) -> isWord
        // If there are chars left - check if the first char is in the children
        | c::cs, Node(_, children) -> 
            match Map.tryFind c children with
            | Some t -> contains' cs t
            | None -> false
    contains' (word |> Seq.toList) trie

let is_prefix (word: string) (trie: Trie) = 
    let rec prefix' (word: char list) (trie: Trie) = 
        match word, trie with
        // If no more chars - return true
        | [], _ -> true
        // If there are chars left - check if the first char is in the children
        | c::cs, Node(_, children) -> 
            match Map.tryFind c children with
            | Some t -> prefix' cs t
            | None -> false
    prefix' (word |> Seq.toList) trie

let prefix (word: string) (trie: Trie) : Trie =
    let rec prefix' (word: char list) (trie: Trie) = 
        match word, trie with
        // If no more chars - return the trie
        | [], _ -> trie
        // If there are chars left - check if the first char is in the children
        | c::cs, Node(_, children) -> 
            match Map.tryFind c children with
            | Some t -> prefix' cs t
            | None -> empty
    prefix' (word |> Seq.toList) trie

//tests
let trie = 
    File.ReadAllLines("English.txt")
    |> Seq.fold (fun trie word -> add word trie) empty  

File.ReadAllLines("English.txt")
|> Seq.iter (fun word -> printfn "%s: %b" word (contains word trie))

["HELLO"; "ASDF"; "WORL"; "WORLDLY"; "WORLDLYNESS"; "WORLDLYNESSES"]
|> Seq.iter (fun word -> printfn "%s: %b" word (contains word trie))