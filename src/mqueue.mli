exception Empty
(** Empty queue. *)

type error = [ `Corrupted | `Invalid_access ]
(** Internal errors. *)

exception Error of error
(** Internal errors. *)

module type S = sig

  include Irmin.Contents.S
  (** The type of queues. *)

  type elt
  (** The elements of the queues. *)

  val create : unit -> t Lwt.t
  (** Create a new queue. *)

  val length : t -> int Lwt.t
  (** Return the length of the queue. *)

  val is_empty : t -> bool Lwt.t
  (** Return true if the given queue is empty, false
      otherwise. *)

  val push : t -> elt -> t Lwt.t
  (** Returns a queue with [value] added to the end of [t]. *)

  val pop_exn : t -> (elt * t) Lwt.t
  (** Returns the top element and a version of the queue where it is removed.
      Raises [Empty] if the queue is empty. *)

  val pop : t -> (elt * t) option Lwt.t
  (** Like pop_exn, but returns None if the queue is empty *)

  val peek_exn : t -> (elt * t) Lwt.t
  (** Returns the head of the queue and a new queue with the head removed.
      Raises [Empty] if no element is found. *)

  val peek : t -> (elt * t) option Lwt.t
  (** Like peek_exn, but returns None if the queue is empty. *)

  val to_list : t -> string list Lwt.t
  (** Return the contents of the queue as a list. *)
end

module type Config = sig
  val conf: Irmin.config
end

module Make
    (AO: Irmin.AO_MAKER)
    (K: Irmin.Hash.S)
    (P: Irmin.Path.S)
    (C: Config)
  : S
