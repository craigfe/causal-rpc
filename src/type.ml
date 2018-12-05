
(* Here type eq has only one constructor, and by matching on it one adds a local
   constraint allowing the conversion between a and b. By building such equality
   witnesses, one can define equality for syntactically different types. *)
type (_, _) eq = Eq: ('a, 'a) eq

(* The primitive types *)
type _ t =
  | Bool      : bool t
  | Bytes     : bytes t
  | Char      : char t
  | Float     : float t
  | Int       : int t
  | Int32     : int32 t
  | Int64     : int64 t
  | String    : string t
  | Unit      : unit t
  | List      : 'a t -> 'a list t

type 'a equal = 'a -> 'a -> bool

(* Define a reflexivity relation on types *)
module Refl = struct

  let rec t: type a b. a t -> b t -> (a, b) eq option = fun a b ->
    match a, b with
    | Bool     , Bool      -> Some Eq
    | Bytes    , Bytes     -> Some Eq
    | Char     , Char      -> Some Eq
    | Float    , Float     -> Some Eq
    | Int      , Int       -> Some Eq
    | Int32    , Int32     -> Some Eq
    | Int64    , Int64     -> Some Eq
    | String   , String    -> Some Eq
    | Unit     , Unit      -> Some Eq
    | List l1, List l2 -> (match t l1 l2 with
        | Some Eq -> Some Eq
        | None -> None)
    | _, _ -> None

end

module Equal = struct
  let list e x y = x == y || (List.length x = List.length y && List.for_all2 e x y)

  let rec t: type a. a t -> a equal = function
    | Unit   -> (=)
    | Bool   -> (=)
    | Bytes  -> (=)
    | Char   -> (=)
    | Float  -> (=) (* N.B. Equality is ill-defined on floats *)
    | Int    -> (=)
    | Int32  -> (=)
    | Int64  -> (=)
    | String -> String.equal
    | List l    -> list (t l)
end

let unit   = Unit
let bool   = Bool
let bytes  = Bytes
let char   = Char
let float  = Float
let int    = Int
let int32  = Int32
let int64  = Int64
let string = String
let list t = List t

let refl = Refl.t
let equal = Equal.t

module Boxed = struct
  type box =
    | Bool of bool
    | Bytes of bytes
    | Char of char
    | Float of float
    | Int of int
    | Int32 of int32
    | Int64 of int64
    | String of string
    | Unit of unit
    | List of box list

  let irmin_t = let open Irmin.Type in
    mu (fun x ->
        variant "irmin_t" (fun bool bytes char float int int32 int64 string list unit -> function
            | Bool b -> bool b
            | Bytes b -> bytes b
            | Char c -> char c
            | Float f -> float f
            | Int i -> int i
            | Int32 i -> int32 i
            | Int64 i -> int64 i
            | String s -> string s
            | List l -> list l
            | Unit u -> unit u)
        |~ case1 "Bool" bool (fun b -> Bool b)
        |~ case1 "Bytes" bytes (fun b -> Bytes b)
        |~ case1 "Char" char (fun c -> Char c)
        |~ case1 "Float" float (fun f -> Float f)
        |~ case1 "Int" int (fun i -> Int i)
        |~ case1 "Int32" int32 (fun i -> Int32 i)
        |~ case1 "Int64" int64 (fun i -> Int64 i)
        |~ case1 "String" string (fun u -> String u)
        |~ case1 "List" (list x) (fun l -> List l)
        |~ case1 "Unit" unit (fun u -> Unit u)
        |> sealv)

  let rec equal a b = match (a, b) with
    | Bool b1, Bool b2 -> (b1 == b2)
    | Bytes b1, Bytes b2 -> (b1 == b2)
    | Char c1, Char c2 -> (c1 == c2)
    | Float f1, Float f2 -> (f1 == f2)
    | Int i1, Int i2 -> (i1 == i2)
    | Int32 i1, Int32 i2 -> (i1 == i2)
    | Int64 i1, Int64 i2 -> (i1 == i2)
    | String s1, String s2 -> String.equal s1 s2
    | Unit (), Unit () -> true
    | List l1, List l2 -> List.fold_right (&&) (List.map2 equal l1 l2) true
    | _ -> false

  let pp ppf v = match v with
    | Bool b -> Fmt.pf ppf "Bool %b" b
    | Bytes b -> Fmt.pf ppf "Bytes %s" (Bytes.to_string b)
    | Char c -> Fmt.pf ppf "Char %c" c
    | Float f -> Fmt.pf ppf "Float %f" f
    | Int i -> Fmt.pf ppf "Int %d" i
    | Int32 i -> Fmt.pf ppf "Int32 %ld" i
    | Int64 i -> Fmt.pf ppf "Int64 %Ld" i
    | String s -> Fmt.pf ppf "String %s" s
    | Unit () -> Fmt.pf ppf "Unit ()"

    | List _ -> invalid_arg "unsupported pretty printer" (* TODO *)

  let test_t = Alcotest.testable pp equal

  exception Type_error

  let rec box: type a. a t -> a -> box = function
    | Unit -> (fun u -> Unit u)
    | Bool -> (fun b -> Bool b)
    | Bytes -> (fun b -> Bytes b)
    | Char -> (fun c -> Char c)
    | Float -> (fun f -> Float f)
    | Int -> (fun i -> Int i)
    | Int32 -> (fun i -> Int32 i)
    | Int64 -> (fun i -> Int64 i)
    | String -> (fun s -> String s)
    | List elt -> (fun l -> List (List.map (box elt) l))

  let rec unbox: type a. a t -> box -> a = fun a b ->
    match (a, b) with
    | (Unit, Unit u) -> u
    | (Bool, Bool b) -> b
    | (Bytes, Bytes b) -> b
    | (Float, Float f) -> f
    | (Char, Char c) -> c
    | (Int, Int i) -> i
    | (Int32, Int32 i) -> i
    | (Int64, Int64 i) -> i
    | (String, String s) -> s
    | (List elt, List l) -> List.map (unbox elt) l
    | _ -> raise Type_error

  type t = box
end
