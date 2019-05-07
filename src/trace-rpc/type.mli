type 'a t

(* An equality test on two values of type 'a *)
type 'a equal = 'a -> 'a -> bool

(** Primitive types *)

val bool: bool t
(** [bool] is a representation of the [boolean] type. *)

val bytes: bytes t
(** [bytes] is a representation of the [bytes] type. *)

val char: char t
(** [char] is a representation of the [char] type. *)

val float: float t
(** [float] is a representation of the [float] type. *)

val int: int t
(** [int] is a representation of the [int] type. *)

val int32: int32 t
(** [int32] is a representation of the [Int32.t] type. *)

val int64: int64 t
(** [int64] is a representation of the [Int64.t] type. *)

val string: string t
(** [string] is a representation of the [string] type. *)

val unit: unit t
(** [unit] is a representation of the [unit] type. *)

(** Basic combinators *)

val pair: 'a t -> 'b t -> ('a * 'b) t
(** [pair a b] is a representation of list of values of type [a * b]. *)

val proj2_1: ('a * 'b) t -> 'a t
(* [proj2_1 pair] is a representation of the first component of [pair] *)

val proj2_2: ('a * 'b) t -> 'b t
(* [proj2_2 pair] is a representation of the second component of [pair] *)

val triple: 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
(** [triple a b c] is a representation of list of values of type [a * b * c]. *)

val array: 'a t -> 'a array t
(** [array t] is a representation of arrays of values of type [t]. *)

val list: 'a t -> 'a list t
(** [list t] is a representation of lists of values of type [t]. *)

val option: 'a t -> 'a option t
(** [option t] is a representation of values of type [t option]. *)

val result: 'a t -> 'b t -> ('a, 'b) result t
(** [result a b] is a representation of values of type [(a, b) result]. *)

val mu: ('a t -> 'a t) -> 'a t
(** [mu f] is the representation of the least fixed point of [f]. That is,
    the least type [t] such that [t = mu t]. *)


type ('a, 'b) case_set
type 'a case_v
type ('a, 'b, 'c) open_variant

val case0: string -> 'a -> ('a, 'a case_v) case_set
val case1: string -> 'b t -> ('b -> 'a) -> ('a, 'b -> 'a case_v) case_set
val sealv: ('a, 'b, 'a -> 'a case_v) open_variant -> 'a t
val variant: string -> 'b -> ('a, 'b, 'b) open_variant

val (|~):
  ('a, 'b, 'c -> 'd) open_variant -> ('a, 'c) case_set -> ('a, 'b, 'd) open_variant

val equal: 'a t -> 'a equal
(** [equal t] is the equality function for values of type [t]. *)

module Boxed : sig
  type box [@@deriving show, eq]
  val irmin_t: box Irmin.Type.t
  val test_t: box Alcotest.testable

  exception Type_error
  val box: 'a t -> 'a -> box
  val unbox: 'a t -> box -> 'a

  type t = box [@@deriving show, eq]
end
