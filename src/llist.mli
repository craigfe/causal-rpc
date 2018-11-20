
type z = Zero
type 'a s = Succ of 'a

type (_, _) t =
  | Nil : (z, 'a) t
  | Cons : ('a * ('l, 'a) t) -> ('l s, 'a) t

val tail: ('l s, 'a) t -> ('l, 'a) t
val head: ('l s, 'a) t -> 'a
