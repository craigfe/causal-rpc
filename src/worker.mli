module type W = sig
  type value
  (** The type of the map values *)

  val run: ?name:string -> ?dir:string -> client:string -> unit -> unit Lwt.t
  (** run ~name ~dir c () returns a promise of a worker computation with a commit
      author ~name that listens for work requests from client c and performs the work
      in repository ~dir *)
end

module Make (Val: Irmin.Contents.S) (Op: Map.Operations with type t = Val.t) : W
  with type value = Val.t
(** Functor building a worker that operates on the map structure
    given an equality type and a set of operations on that type. *)
