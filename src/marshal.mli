(* Each RPC takes a 'a Param.t list as an argument *)
module Param : sig
  type t

  (* TODO: make generic for all Irmin serialisable values *)
  (* TODO: add boxed type to allow listing *)

  (* We require all paramaters to be constructed from Irmin
     serialisable types *)
  val mk: string Irmin.Type.t -> t
end

module Rpc : sig
  type t
  val pp : t Fmt.t
  val of_string : string -> (t, [ `Msg of string ]) result
  val make : string -> string list -> string option -> t
end
