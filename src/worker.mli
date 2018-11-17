module type W = sig
  open Map_contents

  (* --------------- For testing purposes only --------------- *)
  include Map.S

  (** Take a task from the task queue in a store and return it *)
  val get_task_opt: Sync.db -> Irmin.remote -> task option Lwt.t

  (** Given a map and a task to be performed on that map *)
  val perform_task: t -> task -> t

  (** Checkout br_name in store and pull from client, then perform any work still to
      do on that branch *)
  val handle_request: Store.repo -> string -> string -> unit Lwt.t

  (* --------------------------------------------------------- *)

  val run: ?name:string -> ?dir:string -> client:string -> unit -> unit Lwt.t
  (** run ~name ~dir c () returns a promise of a worker computation with a commit
      author ~name that listens for work requests from client c and performs the work
      in repository ~dir *)
end

module Make (Map : Map.S) (Impl: Interface.IMPL with type t = Map.value) : W
(** Functor building a worker that operates on the map structure
    given an equality type and a set of operations on that type. *)
