type 'a implementation
type description

module type DESC = sig
  type t
  val api: description
end

module type IMPL = sig
  type t
  val api: t implementation
end

exception Invalid_definition of string

module type S = sig
  val declare: string list -> description
  val implement: (string * ('a -> 'a)) list -> 'a implementation
end

module Make: S

