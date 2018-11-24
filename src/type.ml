module Param = struct
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
