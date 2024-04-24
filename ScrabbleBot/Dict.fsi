module internal Dict
    
    type Dict =
        | Node of bool * Map<char, Dict>

    val empty : Dict

    val insert : string -> Dict -> Dict

    val lookup : string -> Dict -> bool

    val is_prefix : string -> Dict -> bool

    val prefix : string -> Dict -> Dict

    val step : char -> Dict -> bool * Dict 
