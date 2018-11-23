module Param : sig
  type t =
    | Unit of unit
    | Bool of bool
    | Char of char
    | Int32 of int32
    | Int64 of int64
    | String of string

  val irmin_t: t Irmin.Type.t
end
