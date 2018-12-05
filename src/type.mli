
type 'a t

(* An equality test on two values of type 'a *)
type 'a equal = 'a -> 'a -> bool

(** Primitive types *)
val bool: bool t
val bytes: bytes t
val char: char t
val float: float t
val int: int t
val int32: int32 t
val int64: int64 t
val string: string t
val unit: unit t

(** Basic combinators *)
val array: 'a t -> 'a array t
val list: 'a t -> 'a list t
val option: 'a t -> 'a option t
val result: 'a t -> 'b t -> ('a, 'b) result t
val func: 'a t -> 'b t -> ('a -> 'b) t

type (_, _) eq = Eq: ('a, 'a) eq
val refl: 'a t -> 'b t -> ('a, 'b) eq option
val equal: 'a t -> 'a equal

module Boxed : sig
  type box
  val irmin_t: box Irmin.Type.t
  val test_t: box Alcotest.testable

  exception Type_error
  val box: 'a t -> 'a -> box
  val unbox: 'a t -> box -> 'a

  type t = box
end
