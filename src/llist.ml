
(* Zero and successor *)

type z = Zero
type 'a s = Succ of 'a

(* let rec of_int: type n. int -> 'n = fun nat -> match nat with
 *   | 0 -> Zero
 *   | nat -> Succ (of_int (nat - 1)) *)

type (_, _) t =
  | Nil : (z, 'a) t
  | Cons : ('a * ('l, 'a) t) -> ('l s, 'a) t

let ( --> ) a b = Cons (a, b)

let tail: ('l s, 'a) t -> ('l, 'a) t = function
  | Cons (_, xs) -> xs

let head: type l. (l s, 'a) t -> 'a = function
  | Cons (x, _) -> x
