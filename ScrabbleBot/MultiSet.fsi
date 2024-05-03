// Insert your MultiSet.fsi file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a : comparison>

    val public empty : MultiSet<'a>
    val add   : 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a>
    val fold  : ('b -> 'a -> uint32 -> 'b) -> 'b -> MultiSet<'a> -> 'b
    val remove: 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a>
    val subtract: MultiSet<'a> -> MultiSet<'a> -> MultiSet<'a>
    val union: MultiSet<'a> -> MultiSet<'a> -> MultiSet<'a>
    val toList: MultiSet<'a> -> 'a list
    val ofList: 'a list -> MultiSet<'a>