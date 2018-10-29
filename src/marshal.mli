(* The identifier for an RPC is an Irmin serialisable value *)
module Id : sig
  type t
  val id_of_int : int -> t
  val id_of_int64 : int64 -> t
end

(* Each RPC takes a 'a Param.t list as an argument *)
module Param : sig
  type 'a t

  (* TODO: make generic for all Irmin serialisable values *)
  (* TODO: add boxed type to allow listing *)

  (* We require all paramaters to be constructed from Irmin
     serialisable types *)
  val mk: 'a Irmin.Type.t -> 'a t
end

module Rpc : sig
  type t
  val pp : t Fmt.t
  val of_string : string -> (t, [ `Msg of string ]) result
  val make : Id.t -> string list -> string option -> t
end
