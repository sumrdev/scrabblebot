module Dictionary
    
    type Dict
    val empty : unit -> Dict

    val insert : string -> Dict -> Dict

    val lookup : string -> Dict -> bool

    val is_prefix : string -> Dict -> bool

    val prefix : string -> Dict -> Dict

    val step : char -> Dict -> (bool * Dict ) option
