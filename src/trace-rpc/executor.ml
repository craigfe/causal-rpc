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

  let execute_task ?src boxed_mi params old_value =
    (I.Op.pass_params ?src boxed_mi params) old_value
  end
