open Lwt

exception Empty
type error = [ `Corrupted | `Invalid_access ]
exception Error of error

module type S = sig
  include Irmin.Contents.S
  type elt
  val create : unit -> t Lwt.t
  val length : t -> int Lwt.t
  val is_empty : t -> bool Lwt.t
  val push : t -> elt -> t Lwt.t
  val pop_exn : t -> (elt * t) Lwt.t
  val pop : t -> (elt * t) option Lwt.t
  val peek_exn : t -> (elt * t) Lwt.t
  val peek : t -> (elt * t) option Lwt.t
  val to_list : t -> string list Lwt.t
end

module type Config = sig
  val conf: Irmin.config
end
