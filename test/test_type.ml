open Trace_rpc.Type

(* These helper functions are used to check that each of the box functions
   are reversible. i.e: unbox(box(x)) = x for all x. *)
let rev_box t v = Boxed.(box t v |> unbox t)

type boxed = E: ('a t * 'a Alcotest.testable * 'a * string) -> boxed

let test_rev = function | E (causal_t, alcotest_t, v, typename) ->
  rev_box causal_t v
  |> Alcotest.(check alcotest_t) (typename ^ " pickling is reversible") v

let primitive_test () =
  let module A = Alcotest in
  let tests = [
    E (bool  , A.bool    , true     , "Boolean");
    E (char  , A.char    , 'a'      , "Character");
    E (int   , A.int     , 1        , "Int");
    E (int32 , A.int32   , Int32.one, "Int32");
    E (int64 , A.int64   , Int64.one, "Int64");
    E (float , A.float 1., 1.       , "Float");
    E (string, A.string  , "A"      , "String");
    E (unit  , A.unit    , ()       , "Unit")
  ] in

  List.iter test_rev tests

let triple_pp ?sep:(pp_sep = Fmt.cut) pp_l pp_m pp_r ppf (l, m, r) =
  pp_l ppf l;
  pp_sep ppf ();
  pp_m ppf m;
  pp_sep ppf ();
  pp_r ppf r

(* Define a testable for triples *)
let triple_test l m r =
  let open Alcotest in
  let pp_v = triple_pp (pp l) (pp m) (pp r) in
  let eq_v =
    fun (a, b, c) (d, e, f) ->
      ((equal l) a d) && ((equal m) b e) && ((equal r) c f)
  in
  testable pp_v eq_v

let higher_order_test () =
  let module A = Alcotest in
  let tests = [
    E (pair int string, A.(pair int string), (1, "S"), "Pair");
    E (triple int string bool, A.(triple_test int string bool), (1, "S", true), "Triple");
    E (list int, A.(list int), [1;2;3], "List");
    E (array int, A.(array int), [|1;2;3|], "Array");
    E (option int, A.(option int), Some 1, "Option(Some)");
    E (option int, A.(option int), None  , "Option(None)");
    E (result int string, A.(result int string), Ok 1, "Result(Ok)");
    E (result int string, A.(result int string), Error "S", "Result(Error)");
  ] in

  List.iter test_rev tests;


type var = Unit | Int of int [@@deriving show, eq]
let test_var = Alcotest.testable pp_var equal_var

let variant_test () =
  let v = variant "var" (fun unit int -> function
      | Unit -> unit
      | Int i -> int i)
          |~ case0 "Unit" Unit
          |~ case1 "Int" int (fun i -> Int i)
          |> sealv in

  let tests = [
    E (v, test_var, Unit, "var(Unit)");
    E (v, test_var, Int 1, "var(Int)");
  ] in

  List.iter test_rev tests;

  let v_rev = variant "var" (fun int unit -> function
      | Unit -> unit
      | Int i -> int i)
          |~ case1 "Int" int (fun i -> Int i)
          |~ case0 "Unit" Unit
          |> sealv in

  Boxed.(box v Unit |> unbox v_rev)
  |> Alcotest.(check test_var) "Variants are commutative" Unit


type tree = Leaf | Branch of tree * int * tree [@@deriving show, eq]
let test_tree = Alcotest.testable pp_tree equal_tree

let recursive_test () =
  let tree = mu (fun (x: tree t)  ->
      variant "tree" (fun leaf branch -> function
          | Leaf -> leaf
          | Branch (l, v, r) -> branch (l, v, r))
      |~ case0 "Leaf" Leaf
      |~ case1 "Branch" (triple x int x) (fun (l, v, r) -> Branch (l, v, r))
      |> sealv) in

  (* Helper methods for constructing test trees *)
  let bottom i = Branch(Leaf, i, Leaf) in
  let bottom2 i = Branch(bottom (i-1), i, bottom(i+1)) in
  let bottom3 i = Branch(bottom2 (i-5), i, bottom2(i+5)) in

  let tests = [
    E (tree, test_tree, Leaf, "tree(Leaf)");
    E (tree, test_tree, bottom 1, "tree(Branch)");
    E (tree, test_tree, Branch(bottom 1, 2, Leaf), "tree(Branch)");
    E (tree, test_tree, Branch(bottom2 2, 4, Leaf), "tree(Branch)");
    E (tree, test_tree, Branch(bottom3 1, 10, bottom3 20), "tree(Branch)");
  ] in
  List.iter test_rev tests

let tests = [
  Alcotest.test_case "Primitive types" `Quick primitive_test;
  Alcotest.test_case "Higher-order types" `Quick higher_order_test;
  Alcotest.test_case "Recursive types" `Quick recursive_test;
]
