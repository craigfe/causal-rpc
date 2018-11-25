module Boxed = struct
  type t =
    | Unit of unit
    | Bool of bool
    | Char of char
    | Int32 of int32
    | Int64 of int64
    | String of string

  let irmin_t = let open Irmin.Type in
    variant "irmin_t" (fun unit bool char int32 int64 string -> function
        | Unit u -> unit u
        | Bool b -> bool b
        | Char c -> char c
        | Int32 i -> int32 i
        | Int64 i -> int64 i
        | String s -> string s
      )
  |~ case1 "Unit" Irmin.Type.unit (fun u -> Unit u)
  |~ case1 "Bool" Irmin.Type.bool (fun b -> Bool b)
  |~ case1 "Char" Irmin.Type.char (fun c -> Char c)
  |~ case1 "Int32" Irmin.Type.int32 (fun i -> Int32 i)
  |~ case1 "Int64" Irmin.Type.int64 (fun i -> Int64 i)
  |~ case1 "String" Irmin.Type.string (fun u -> String u)
  |> sealv

  let equal a b = match (a, b) with
    | Unit (), Unit () -> true
    | Bool b1, Bool b2 -> (b1 == b2)
    | Char c1, Char c2 -> (c1 == c2)
    | Int32 i1, Int32 i2 -> (i1 == i2)
    | Int64 i1, Int64 i2 -> (i1 == i2)
    | String s1, String s2 -> (s1 == s2)
    | _ -> false

  let pp ppf v = match v with
      | Unit () -> Fmt.pf ppf "Unit ()"
      | Bool b -> Fmt.pf ppf "Bool %b" b
      | Char c -> Fmt.pf ppf "Char %c" c
      | Int32 i -> Fmt.pf ppf "Int32 %ld" i
      | Int64 i -> Fmt.pf ppf "Int64 %Ld" i
      | String s -> Fmt.pf ppf "String %s" s

  let test_t = Alcotest.testable pp equal
end

(* Here type eq has only one constructor, and by matching on it one adds a local
   constraint allowing the conversion between a and b. By building such equality
   witnesses, one can define equality for syntactically different types. *)
type (_, _) eq = Eq: ('a, 'a) eq


(* An equality test on two values of type 'a *)
type 'a equal = 'a -> 'a -> bool

(* The primitive types *)
type 'a t =
  | Unit   : unit t
  | Bool   : bool t
  | Char   : char t
  | Int32  : int32 t
  | Int64  : int64 t
  | String : string t

(* Define a reflexivity relation on types *)
module Refl = struct

  let t: type a b. a t -> b t -> (a, b) eq option = fun a b ->
    match a, b with
    | Unit  , Unit   -> Some Eq
    | Bool  , Bool   -> Some Eq
    | Char  , Char   -> Some Eq
    | Int32 , Int32  -> Some Eq
    | Int64 , Int64  -> Some Eq
    | String, String -> Some Eq
    | _ -> None

end

let unit   = Unit
let bool   = Bool
let char   = Char
let int32  = Int32
let int64  = Int64
let string = String

let to_boxed: type a. a t -> a -> Boxed.t = function
  | Unit -> (fun u -> Boxed.Unit u)
  | Bool -> (fun b -> Boxed.Bool b)
  | Char -> (fun c -> Boxed.Char c)
  | Int32 -> (fun i -> Boxed.Int32 i)
  | Int64 -> (fun i -> Boxed.Int64 i)
  | String -> (fun s -> Boxed.String s)

exception Type_error
let from_boxed: type a. a t -> Boxed.t -> a = fun a b ->
  match (a, b) with
  | (Unit, Boxed.Unit u) -> u
  | (Bool, Boxed.Bool b) -> b
  | (Char, Boxed.Char c) -> c
  | (Int32, Boxed.Int32 i) -> i
  | (Int64, Boxed.Int64 i) -> i
  | (String, Boxed.String s) -> s
  | _ -> raise Type_error

let to_irmin_type: type a. a t -> a Irmin.Type.t = function
  | Unit   -> Irmin.Type.unit
  | Bool   -> Irmin.Type.bool
  | Char   -> Irmin.Type.char
  | Int32  -> Irmin.Type.int32
  | Int64  -> Irmin.Type.int64
  | String -> Irmin.Type.string

let to_testable: type a. a t -> a Alcotest.testable = function
  | Unit   -> Alcotest.unit
  | Bool   -> Alcotest.bool
  | Char   -> Alcotest.char
  | Int32  -> Alcotest.int32
  | Int64  -> Alcotest.int64
  | String -> Alcotest.string

module Equal = struct
  let unit _ _ = true
  let bool  (x:bool) (y:bool) = x = y
  let char  (x:char) (y:char) = x = y
  let int32 (x:int32) (y:int32) = x = y
  let int64 (x:int64) (y:int64) = x = y
  let string x y = x == y || String.equal x y

  let t: type a. a t -> a equal = function
    | Unit   -> unit
    | Bool   -> bool
    | Char   -> char
    | Int32  -> int32
    | Int64  -> int64
    | String -> string
end

let refl = Refl.t
let equal = Equal.t
