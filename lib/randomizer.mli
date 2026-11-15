open Types

type t

val all_piece_kinds : piece_kind list
val modulus : int
val create : seed:int -> t
val next : t -> piece_kind * t
