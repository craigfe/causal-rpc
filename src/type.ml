
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
  | Array     : 'a t -> 'a array t
  | List      : 'a t -> 'a list t
  | Option    : 'a t -> 'a option t
  | Result    : ('a t * 'b t) -> ('a, 'b) result t

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
    | Array a1, Array a2 -> (match t a1 a2 with
        | Some Eq -> Some Eq
        | None -> None)
    | List l1, List l2 -> (match t l1 l2 with
        | Some Eq -> Some Eq
        | None -> None)
    | Option o1, Option o2 -> (match t o1 o2 with
        | Some Eq -> Some Eq
        | None -> None)
    | Result (o1, e1), Result (o2, e2) -> (match (t o1 o2, t e1 e2) with
        | (Some Eq, Some Eq) -> Some Eq
        | _ -> None)
    | _, _ -> None

end

module Equal = struct
  let list e x y = x == y || (List.length x = List.length y && List.for_all2 e x y)

  let array e x y =
    Array.mapi (fun i a -> e a (Array.get y i)) x
    |> Array.for_all (fun x -> x)

  let option e x y = match (x, y) with
    | Some a, Some b -> e a b
    | None, None -> true
    | _ -> false

  let result o_comp e_comp x y = match (x, y) with
    | Ok o1, Ok o2 -> o_comp o1 o2
    | Error e1, Error e2 -> e_comp e1 e2
    | _ -> false

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
    | Array a -> array (t a)
    | List l  -> list (t l)
    | Option o -> option (t o)
    | Result (o, e) -> result (t o) (t e)
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
let array t = Array t
let list t = List t
let option t = Option t
let result o e = Result (o, e)

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
    | Array of box array
    | List of box list
    | Option of box option
    | Result of (box, box) result

  let irmin_t = let open Irmin.Type in
    mu (fun x -> variant "irmin_t"
           (fun bool bytes char float int int32
             int64 string unit array list option
             result -> function
             | Bool b -> bool b
             | Bytes b -> bytes b
             | Char c -> char c
             | Float f -> float f
             | Int i -> int i
             | Int32 i -> int32 i
             | Int64 i -> int64 i
             | String s -> string s
             | Unit u -> unit u
             | Array a -> array a
             | List l -> list l
             | Option o -> option o
             | Result r -> result r)
                 |~ case1 "Bool" bool (fun b -> Bool b)
                 |~ case1 "Bytes" bytes (fun b -> Bytes b)
                 |~ case1 "Char" char (fun c -> Char c)
                 |~ case1 "Float" float (fun f -> Float f)
                 |~ case1 "Int" int (fun i -> Int i)
                 |~ case1 "Int32" int32 (fun i -> Int32 i)
                 |~ case1 "Int64" int64 (fun i -> Int64 i)
                 |~ case1 "String" string (fun u -> String u)
                 |~ case1 "Unit" unit (fun u -> Unit u)
                 |~ case1 "Array" (array x) (fun a -> Array a)
                 |~ case1 "List" (list x) (fun l -> List l)
                 |~ case1 "Option" (option x) (fun o -> Option o)
                 |~ case1 "Result" (result x x) (fun r -> Result r)
                 |> sealv)

  (* TODO: don't implement this twice. Use Equal.t instead *)
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
    | Array x, Array y ->
      Array.mapi (fun i a -> equal a (Array.get y i)) x
      |> Array.for_all (fun x -> x)

    | List l1, List l2 -> List.fold_right (&&) (List.map2 equal l1 l2) true
    | Option o1, Option o2 -> (match (o1, o2) with
        | Some a, Some b -> equal a b
        | None, None -> true
        | _ -> false)
    | Result r1, Result r2 -> (match (r1, r2) with
        | Ok a, Ok b -> equal a b
        | Error a, Error b -> equal a b
        | _ -> false)
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

    (* TODO *)
    | Array _ -> invalid_arg "unsupported pretty printer"
    | List _ -> invalid_arg "unsupported pretty printer"
    | Result _ -> invalid_arg "unsupported pretty printer"
    | Option _ -> invalid_arg "unsupported pretty printer"

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
    | Array elt -> (fun a -> Array (Array.map (box elt) a))
    | List elt -> (fun l -> List (List.map (box elt) l))
    | Result (a, b) -> (fun r -> match r with
        | Ok o -> Result (Ok (box a o))
        | Error e -> Result (Error (box b e)))
    | Option opt -> (fun o -> match o with
        | Some s -> Option (Some (box opt s))
        | None -> Option None)

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
    | (Array elt, Array a) -> Array.map (unbox elt) a
    | (List elt, List l) -> List.map (unbox elt) l
    | (Result (o, _), Result (Ok x)) -> Ok (unbox o x)
    | (Result (_, e), Result (Error x)) -> Error (unbox e x)
    | (Option o, Option (Some s)) -> Some (unbox o s)
    | (Option _, Option None) -> None
    | _ -> raise Type_error

  type t = box
end
