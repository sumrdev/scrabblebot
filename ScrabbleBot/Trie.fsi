module internal Trie
    
    type Trie =
        | Node of bool * Map<char, Trie>

    val empty : Trie

    val add : string -> Trie -> Trie

    val contains : string -> Trie -> bool

    val is_prefix : string -> Trie -> bool

    val prefix : string -> Trie -> Trie
