module type W = sig
  val run:
    ?switch:Lwt_switch.t ->
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
