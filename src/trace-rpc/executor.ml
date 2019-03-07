module type S = sig
  module I: Interface.IMPL_MAKER
  type value = I.S.t

  val initialise: ?src:Logs.src -> thread_count:int -> unit
  val execute_task: ?src:Logs.src -> I.Op.boxed_mi -> Type.Boxed.t list -> value -> value Lwt.t
end

module Make(I: Interface.IMPL_MAKER): S with module I = I = struct
  module I = I
  type value = I.S.t

  let initialise ?src ~thread_count = ()
    (* let log = fun s -> (Logs.warn ?src (fun m -> m "%s" s)) in
     * Lwt_preemptive.init thread_count thread_count log *)

  (* We have a function of type (param -> ... -> param -> val -> val).
     Here we take the parameters that were passed as part of the RPC and recursively apply them
     to the function implementation until we are left with a function of type (val -> val). *)
  let pass_params ?src boxed_mi params =
    match boxed_mi with
    | I.Op.E matched_impl ->
      let (unboxed, func) = matched_impl in
      let func_type = Interface.NamedOp.typ unboxed in

      (* We take a function type and a function _of that type_, and recursively apply parameters
         to the function until it reaches 'BaseType', i.e. val -> val *)
      let rec aux: type a.
        (value, a) Interface.prototype
        -> a
        -> Type.Boxed.t list
        -> (value -> value) = fun func_type func params ->

        match func_type with
        | Interface.BaseType -> (Logs.debug ?src (fun m -> m "Reached base type"); match params with
          | [] -> (fun x ->
              Logs.debug ?src (fun m -> m "Executing val -> val level function");
              let v = func x in
              Logs.debug ?src (fun m -> m "Function execution complete");
              v)
          | _ -> raise @@ Map.Malformed_params "Too many parameters")

        | Interface.ParamType (typ, nested_type) -> (Logs.debug ?src (fun m -> m "Nested type"); match params with
          | (x::xs) -> aux nested_type (func (Type.Boxed.unbox typ x)) xs
          | [] -> raise @@ Map.Malformed_params "Not enough parameters")

      in aux func_type func params

  let execute_task ?src boxed_mi params old_value =
    let func = fun () -> pass_params ?src boxed_mi params old_value in

    Lwt.return (func ())
    (* Lwt_preemptive.detach func () *)
  end
