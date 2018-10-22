val generate_random_name: ?length:int -> unit -> string

val run: ?name:string -> int -> unit Lwt.t
val main: string option -> int -> unit