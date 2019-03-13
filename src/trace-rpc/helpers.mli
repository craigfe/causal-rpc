val handle_merge_conflict: string -> string -> (unit, [ `Conflict of string ]) result -> unit Lwt.t

val generate_rand_string: ?length:int -> unit -> string
val zip: 'a list -> 'b list -> ('a * 'b) list

val split_sequential: int -> 'a list -> 'a list * 'a list
val split_random: int -> 'a list -> 'a list * 'a list

val timestamp: unit -> string
val set_reporter: unit -> unit
val check_within_tmp: string -> unit
