type (_, _) eq = Eq: ('a, 'a) eq
(** Type eq has only one constructor. Matching on it adds a local constraint
    allowing the conversion between a and b. By building such equality witnesses,
    one can define equality for syntactically different types. *)

type 'a t

val make : unit -> 'a t

val eq : 'a t -> 'b t -> ('a, 'b) eq option
