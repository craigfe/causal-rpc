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
  | Pair      : 'a t * 'b t -> ('a * 'b) t
  | Triple    : 'a t * 'b t * 'c t -> ('a * 'b * 'c) t
  | Array     : 'a t -> 'a array t
  | List      : 'a t -> 'a list t
  | Option    : 'a t -> 'a option t
  | Result    : ('a t * 'b t) -> ('a, 'b) result t
  | Variant   : 'a variant -> 'a t
  | Recursive : 'a recur -> 'a t

and 'a variant = {
  witness : 'a Witness.t;
  name    : string;
  cases   : 'a case array;
  get     : 'a -> 'a case_v;
}

and 'a case =
  | Case0: 'a case0 -> 'a case
  | Case1: ('a, 'b) case1 -> 'a case

and 'a case_v =
  | CaseVal0: 'a case0 -> 'a case_v
  | CaseVal1: ('a, 'b) case1 * 'b -> 'a case_v

(* Base constructor of a variant type *)
and 'a case0 = {
  id0   : int;
  name0 : string;
  cons0 : 'a;
}

(* Unary constructor of a variant type *)
and ('a, 'b) case1 = {
  id1  : int;
  name1: string;
  type1: 'b t;
  cons1: 'b -> 'a;
}

and 'a recur = { mutable recur: 'a t }

type 'a equal = 'a -> 'a -> bool

(* (\* Define a reflexivity relation on types *\)
 * module Refl = struct
 * 
 *   let rec t: type a b. a t -> b t -> (a, b) eq option = fun a b ->
 *     match a, b with
 *     | Recursive a, b       -> t a.recur b
 *     | a, Recursive b       -> t a b.recur
 *     | Bool     , Bool      -> Some Eq
 *     | Bytes    , Bytes     -> Some Eq
 *     | Char     , Char      -> Some Eq
 *     | Float    , Float     -> Some Eq
 *     | Int      , Int       -> Some Eq
 *     | Int32    , Int32     -> Some Eq
 *     | Int64    , Int64     -> Some Eq
 *     | String   , String    -> Some Eq
 *     | Unit     , Unit      -> Some Eq
 *     | Array a1, Array a2 -> (match t a1 a2 with
 *         | Some Eq -> Some Eq
 *         | None -> None)
 *     | List l1, List l2 -> (match t l1 l2 with
 *         | Some Eq -> Some Eq
 *         | None -> None)
 *     | Option o1, Option o2 -> (match t o1 o2 with
 *         | Some Eq -> Some Eq
 *         | None -> None)
 *     | Result (o1, e1), Result (o2, e2) -> (match (t o1 o2, t e1 e2) with
 *         | (Some Eq, Some Eq) -> Some Eq
 *         | _ -> None)
 *     | _, _ -> None
 * 
 * end *)

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

  let pair l_comp r_comp (l, r) (l', r')
    = (l_comp l l') && (r_comp r r')

  let triple l_comp m_comp r_comp (l, m, r) (l', m', r')
    = (l_comp l l') && (m_comp m m') && (r_comp r r')

  let variant: type a. a variant -> a equal = fun v x y ->
    match (v.get x, v.get y) with
    | CaseVal0 x      , CaseVal0 y        -> x.id0 = y.id0
    | CaseVal1 (x, vs), CaseVal1 (y, vs') -> x.id1 = y.id1 (* TODO: check type of constructors? *)
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
    | Pair (l, r)      -> pair (t l) (t r)
    | Triple (l, m, r) -> triple (t l) (t m) (t r)
    | Array a          -> array (t a)
    | List l           -> list (t l)
    | Option o         -> option (t o)
    | Result (o, e)    -> result (t o) (t e)
    | Variant v        -> variant v
    | Recursive r -> t r.recur
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
let pair l r = Pair (l, r)
let triple l m r = Triple (l, m, r)
let array t = Array t
let list t = List t
let option t = Option t
let result o e = Result (o, e)
let mu: type a. (a t -> a t) -> a t = fun f ->
  let rec x = { recur = Recursive x } in
  let x' = f (Recursive x) in
  x.recur <- x'; x'

type ('a, 'b) case_set = int -> ('a case * 'b)
type ('a, 'b, 'c) open_variant = 'a case list -> string * 'c * 'a case list

let case0 name0 cons0 id0 =
  let c = { id0; name0; cons0 } in
  (Case0 c, CaseVal0 c)

let case1 name1 type1 cons1 id1 =
  let c = { id1; name1; type1; cons1 } in
  (Case1 c, fun v -> CaseVal1 (c, v))

let variant n c vs = n, c, vs

(* let refl = Refl.t *)
let equal = Equal.t
let app v c cs =
  let n, fc, cs = v cs in
  let c, f = c (List.length cs) in
  n, fc f, (c :: cs)

let sealv v =
  let name, get, cases = v [] in
  let witness = Witness.make () in
  let cases = Array.of_list (List.rev cases) in
  Variant { witness; name; cases ; get }

let (|~) = app

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
    | Pair of box * box
    | Triple of box * box * box
    | Array of box array
    | List of box list
    | Option of box option
    | Result of (box, box) result
    | BoxCase0 of string
    | BoxCase1 of string * box [@@deriving show, eq]

  let irmin_t = let open Irmin.Type in
    mu (fun x -> variant "irmin_t"
           (fun bool bytes char float int int32
             int64 string unit pair triple array
             list option result v_case0 v_case1 -> function
             | Bool b -> bool b
             | Bytes b -> bytes b
             | Char c -> char c
             | Float f -> float f
             | Int i -> int i
             | Int32 i -> int32 i
             | Int64 i -> int64 i
             | String s -> string s
             | Unit u -> unit u
             | Pair (l, r) -> pair (l, r)
             | Triple (l, m, r) -> triple (l, m, r)
             | Array a -> array a
             | List l -> list l
             | Option o -> option o
             | Result r -> result r
             | BoxCase0 s -> v_case0 s
             | BoxCase1 (s, v) -> v_case1 (s, v))
                 |~ case1 "Bool" bool (fun b -> Bool b)
                 |~ case1 "Bytes" bytes (fun b -> Bytes b)
                 |~ case1 "Char" char (fun c -> Char c)
                 |~ case1 "Float" float (fun f -> Float f)
                 |~ case1 "Int" int (fun i -> Int i)
                 |~ case1 "Int32" int32 (fun i -> Int32 i)
                 |~ case1 "Int64" int64 (fun i -> Int64 i)
                 |~ case1 "String" string (fun u -> String u)
                 |~ case1 "Unit" unit (fun u -> Unit u)
                 |~ case1 "Pair" (pair x x) (fun (l, r) -> Pair (l, r))
                 |~ case1 "Triple" (triple x x x) (fun (l, m, r) -> Triple (l, m, r))
                 |~ case1 "Array" (array x) (fun a -> Array a)
                 |~ case1 "List" (list x) (fun l -> List l)
                 |~ case1 "Option" (option x) (fun o -> Option o)
                 |~ case1 "Result" (result x x) (fun r -> Result r)
                 |~ case1 "Case0" string (fun s -> BoxCase0 s)
                 |~ case1 "Case1" (pair string x) (fun (s, v) -> BoxCase1 (s, v))
                 |> sealv)

  let pp_cust ppf = function
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
    | Pair (_, _) -> invalid_arg "unsupported pretty printer"
    | Triple (_, _, _) -> invalid_arg "unsupported pretty printer"
    | Array _ -> invalid_arg "unsupported pretty printer"
    | List _ -> invalid_arg "unsupported pretty printer"
    | Result _ -> invalid_arg "unsupported pretty printer"
    | Option _ -> invalid_arg "unsupported pretty printer"
    | BoxCase0 _ -> invalid_arg "unsupported pretty printer"
    | BoxCase1 _ -> invalid_arg "unsupported pretty printer"

  let test_t = Alcotest.testable pp_cust equal_box

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
    | Pair (l, r) -> (fun (lv, rv) ->
        Pair ((box l) lv, (box r) rv))
    | Triple (l, m, r) -> (fun (lv, mv, rv) ->
        Triple ((box l) lv, (box m) mv, (box r) rv))
    | Array elt -> (fun a -> Array (Array.map (box elt) a))
    | List elt -> (fun l -> List (List.map (box elt) l))
    | Result (a, b) -> (function
        | Ok o -> Result (Ok (box a o))
        | Error e -> Result (Error (box b e)))
    | Option opt -> (function
        | Some s -> Option (Some (box opt s))
        | None -> Option None)
    | Variant v_typ -> (fun v -> match v_typ.get v with
        | CaseVal0 c -> BoxCase0 c.name0
        | CaseVal1 (c, v) -> BoxCase1 (c.name1, box c.type1 v))
    | Recursive r -> box r.recur

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
    | (Pair (l, r), Pair (lv, rv)) -> ((unbox l lv), (unbox r rv))
    | (Triple (l, m, r), Triple (lv, mv, rv)) -> ((unbox l lv), (unbox m mv), (unbox r rv))
    | (Array elt, Array a) -> Array.map (unbox elt) a
    | (List elt, List l) -> List.map (unbox elt) l
    | (Result (o, _), Result (Ok x)) -> Ok (unbox o x)
    | (Result (_, e), Result (Error x)) -> Error (unbox e x)
    | (Option o, Option (Some s)) -> Some (unbox o s)
    | (Option _, Option None) -> None
    | (Variant v_typ, BoxCase0 s) -> (
        let rec aux i =
          if i >= Array.length v_typ.cases
          then raise Type_error
          else match v_typ.cases.(i) with
            | Case0 c when String.equal c.name0 s -> c.cons0
            | _ -> aux (i+1)
        in aux 0)
    | (Variant v_typ, BoxCase1 (s, v)) -> (
        let rec aux i =
          if i >= Array.length v_typ.cases
          then raise Type_error
          else match v_typ.cases.(i) with
            | Case1 c when String.equal c.name1 s -> c.cons1 (unbox c.type1 v)
            | _ -> aux (i+1)
      in aux 0)
    | (Recursive r, v) -> unbox r.recur v
    | _ -> raise Type_error

  type t = box [@@deriving show, eq]
end
