(** The type of job lists. This needs to be exposed by the interface,
    because the Store module needs to produce a value of this type from
    the contents of the store *)
type t = Job.t list

(** Job lists are Irmin serialisable *)
val t: t Irmin.Type.t

(** Test if the job queue is empty*)
val is_empty: t -> bool

(** Add a job to a queue *)
val push: Job.t -> t -> t

(** Remove the first job from a queue. Return Error if the queue is empty,
    otherwise the pair (j,js) where j and js are the head and tail of the original
    queue *)
val pop: t -> (Job.t * t, string) result

(** Return the first job from a queue, otherwise None *)
val peek_opt: t -> Job.t option
