type TrieNode<'T> =
    { Value: 'T option
      Children: Map<char, TrieNode<'T>> }

type Trie<'T> =
    { Root: TrieNode<'T> }

let emptyTrie<'T> : Trie<'T> =
    { Root = { Value = None; Children = Map.empty } }

let rec insert<'T> (word: string) (value: 'T) (node: TrieNode<'T>) : TrieNode<'T> =
    match word with
    | "" -> { node with Value = Some value }
    | _ ->
        let char = word.[0]
        let rest = word.[1..]
        let childNode =
            match Map.tryFind char node.Children with
            | Some child -> child
            | None -> { Value = None; Children = Map.empty }
        let updatedChild = insert rest value childNode
        let updatedChildren = Map.add char updatedChild node.Children
        { node with Children = updatedChildren }

let insertWord<'T> (word: string) (value: 'T) (trie: Trie<'T>) : Trie<'T> =
    { trie with Root = insert word value trie.Root }

let rec find<'T> (word: string) (node: TrieNode<'T>) : 'T option =
    match word with
    | "" -> node.Value
    | _ ->
        let char = word.[0]
        let rest = word.[1..]
        match Map.tryFind char node.Children with
        | Some child -> find rest child
        | None -> None
module ScrabbleBot.Trie

type TrieNode<'T> =
    { Value: 'T option
      Children: Map<char, TrieNode<'T>> }

type Trie<'T> =
    { Root: TrieNode<'T> }

let emptyTrie<'T> : Trie<'T> =
    { Root = { Value = None; Children = Map.empty } }

let rec insert<'T> (word: string) (value: 'T) (node: TrieNode<'T>) : TrieNode<'T> =
    match word with
    | "" -> { node with Value = Some value }
    | _ ->
        let char = word.[0]
        let rest = word.[1..]
        let child = 
            match Map.tryFind char node.Children with
            | Some existingChild -> existingChild
            | None -> { Value = None; Children = Map.empty }
        let updatedChild = insert rest value child
        let updatedChildren = Map.add char updatedChild node.Children
        { node with Children = updatedChildren }

let insertWord<'T> (word: string) (value: 'T) (trie: Trie<'T>) : Trie<'T> =
    { trie with Root = insert word value trie.Root }

let rec find<'T> (word: string) (node: TrieNode<'T>) : 'T option =
    match word with
    | "" -> node.Value
    | _ ->
        let char = word.[0]
        let rest = word.[1..]
        match Map.tryFind char node.Children with
        | Some child -> find rest child
        | None -> None
