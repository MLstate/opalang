module type STUFF =
  sig
    type t
    val get : t -> int -> char
    val set : t -> int -> char -> unit
  end
module StuffF :
  functor (S : STUFF) ->
    sig
      val lei32 : S.t -> int -> int -> unit
      val bei32 : S.t -> int -> int -> unit
      val lei64 : S.t -> int -> int -> unit
      val bei64 : S.t -> int -> int -> unit
      val led : S.t -> int -> float -> unit
      val bed : S.t -> int -> float -> unit
      val lei32l : S.t -> int -> int32 -> unit
      val bei32l : S.t -> int -> int32 -> unit
      val ldi32 : S.t -> int -> int
      val bdi32 : S.t -> int -> int
      val ldi64 : S.t -> int -> int
      val bdi64 : S.t -> int -> int
      val ldd : S.t -> int -> float
      val bdd : S.t -> int -> float
    end
