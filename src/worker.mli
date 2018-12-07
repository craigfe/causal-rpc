module type W = sig
  open Map

  (* --------------- For testing purposes only --------------- *)
  include Map.S

  (** Given a map and a task to be performed on that map *)
  val perform_task: Sync.db -> task -> string -> Sync.db Lwt.t

  (** Checkout br_name in store and pull from client, then perform any work still to
      do on that branch *)
  val handle_request: ?src:Logs.src -> Store.repo -> string -> JobQueue.job -> string -> unit Lwt.t

  (* --------------------------------------------------------- *)

  val run:
    ?log_source:bool ->
    ?name:string ->
    ?dir:string ->
    ?poll_freq:float ->
    client:string -> unit -> unit Lwt.t
    (** run ~name ~dir c () returns a promise of a worker computation with a commit
        author ~name that listens for work requests from client c and performs the work
        in repository ~dir *)
end

module Make (M : Map.S) (Impl: Interface.IMPL with module Val = M.Value) : W
(** Functor building a worker that operates on the map structure
    given an equality type and a set of operations on that type. *)
