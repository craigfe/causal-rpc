
(* Here type eq has only one constructor, and by matching on it one adds a local
   constraint allowing the conversion between a and b. By building such equality
   witnesses, one can define equality for syntactically different types. *)
type (_, _) eq = Eq: ('a, 'a) eq


(* An equality test on two values of type 'a *)
type 'a equal = 'a -> 'a -> bool

(* The primitive types *)
type 'a prim =
  | Unit   : unit prim
  | Bool   : bool prim
  | Char   : char prim
  | Int32  : int32 prim
  | Int64  : int64 prim
  | String : string prim

type 'a t =
  | Prim : 'a prim -> 'a t
  | List : 'a t -> 'a list t

(* Define a reflexivity relation on types *)
module Refl = struct

  let prim: type a b. a prim -> b prim -> (a, b) eq option = fun a b ->
    match a, b with
    | Unit  , Unit   -> Some Eq
    | Bool  , Bool   -> Some Eq
    | Char  , Char   -> Some Eq
    | Int32 , Int32  -> Some Eq
    | Int64 , Int64  -> Some Eq
    | String, String -> Some Eq
    | _ -> None

  let rec t: type a b. a t -> b t -> (a, b) eq option = fun a b ->
    match a, b with
    | Prim p , Prim q  -> prim p q
    | List l1, List l2 -> (match t l1 l2 with
      | Some Eq -> Some Eq
      | None -> None)
    | _, _ -> None

end

let unit   = Prim Unit
let bool   = Prim Bool
let char   = Prim Char
let int32  = Prim Int32
let int64  = Prim Int64
let string = Prim String
let list t = List t


let prim_to_irmin_type: type a. a prim -> a Irmin.Type.t =
  let open Irmin.Type in function
  | Unit   -> unit
  | Bool   -> bool
  | Char   -> char
  | Int32  -> int32
  | Int64  -> int64
  | String -> string

let rec to_irmin_type: type a. a t -> a Irmin.Type.t = function
  | Prim p -> prim_to_irmin_type p
  | List elt -> Irmin.Type.list (to_irmin_type elt)

let prim_to_testable: type a. a prim -> a Alcotest.testable =
  let open Alcotest in function
    | Unit   -> unit
    | Bool   -> bool
    | Char   -> char
    | Int32  -> int32
    | Int64  -> int64
    | String -> string

let rec to_testable: type a. a t -> a Alcotest.testable = function
  | Prim p -> prim_to_testable p
  | List elt -> Alcotest.list (to_testable elt)

module Equal = struct
  let unit _ _ = true
  let bool  (x:bool) (y:bool) = x = y
  let char  (x:char) (y:char) = x = y
  let int32 (x:int32) (y:int32) = x = y
  let int64 (x:int64) (y:int64) = x = y
  let string x y = x == y || String.equal x y
  let list e x y = x == y || (List.length x = List.length y && List.for_all2 e x y)

  let prim: type a. a prim -> a equal = function
    | Unit   -> unit
    | Bool   -> bool
    | Char   -> char
    | Int32  -> int32
    | Int64  -> int64
    | String -> string

  let rec t: type a. a t -> a equal = function
    | Prim p    -> prim p
    | List l    -> list (t l)
end

let refl = Refl.t
let equal = Equal.t

module Boxed = struct
  type box =
    | List of box list
    | Unit of unit
    | Bool of bool
    | Char of char
    | Int32 of int32
    | Int64 of int64
    | String of string

  let irmin_t = let open Irmin.Type in
    mu (fun x -> variant "irmin_t" (fun list unit bool char int32 int64 string -> function
        | List l -> list l
        | Unit u -> unit u
        | Bool b -> bool b
        | Char c -> char c
        | Int32 i -> int32 i
        | Int64 i -> int64 i
        | String s -> string s
      )
  |~ case1 "List" (list x) (fun l -> List l)
  |~ case1 "Unit" unit (fun u -> Unit u)
  |~ case1 "Bool" bool (fun b -> Bool b)
  |~ case1 "Char" char (fun c -> Char c)
  |~ case1 "Int32" int32 (fun i -> Int32 i)
  |~ case1 "Int64" int64 (fun i -> Int64 i)
  |~ case1 "String" string (fun u -> String u)
  |> sealv)

  let rec equal a b = match (a, b) with
    | List l1, List l2 -> List.fold_right (&&) (List.map2 equal l1 l2) true
    | Unit (), Unit () -> true
    | Bool b1, Bool b2 -> (b1 == b2)
    | Char c1, Char c2 -> (c1 == c2)
    | Int32 i1, Int32 i2 -> (i1 == i2)
    | Int64 i1, Int64 i2 -> (i1 == i2)
    | String s1, String s2 -> (s1 == s2)
    | _ -> false

  let pp ppf v = match v with
    | List _ -> Fmt.pf ppf "List" (* TODO: implement pretty printing properly *)
    | Unit () -> Fmt.pf ppf "Unit ()"
    | Bool b -> Fmt.pf ppf "Bool %b" b
    | Char c -> Fmt.pf ppf "Char %c" c
    | Int32 i -> Fmt.pf ppf "Int32 %ld" i
    | Int64 i -> Fmt.pf ppf "Int64 %Ld" i
    | String s -> Fmt.pf ppf "String %s" s

  let test_t = Alcotest.testable pp equal
  exception Type_error

  let prim_box: type a. a prim -> a -> box = function
    | Unit -> (fun u -> Unit u)
    | Bool -> (fun b -> Bool b)
    | Char -> (fun c -> Char c)
    | Int32 -> (fun i -> Int32 i)
    | Int64 -> (fun i -> Int64 i)
    | String -> (fun s -> String s)

  let rec box: type a. a t -> a -> box = function
    | Prim p -> prim_box p
    | List elt -> (fun l -> List (List.map (box elt) l))

  let prim_unbox: type a. a prim -> box -> a = fun a b ->
    match (a, b) with
    | (Unit, Unit u) -> u
    | (Bool, Bool b) -> b
    | (Char, Char c) -> c
    | (Int32, Int32 i) -> i
    | (Int64, Int64 i) -> i
    | (String, String s) -> s
    | _ -> raise Type_error

  let rec unbox: type a. a t -> box -> a = fun a b ->
    match (a, b) with
    | (Prim p_typ, p) -> prim_unbox p_typ p
    | (List elt, List l) -> List.map (unbox elt) l
    | _ -> raise Type_error

  type t = box
end
