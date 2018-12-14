module type S = sig
  module I: Interface.IMPL_MAKER
  type value = I.S.t

  val initialise: ?src:Logs.src -> thread_count:int -> unit
  val execute_task: ?src:Logs.src -> I.Op.boxed_mi -> Type.Boxed.t list -> value -> value Lwt.t
end

module Make(I: Interface.IMPL_MAKER): S with module I = I
