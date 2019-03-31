(* We define equality as an extensible variant type *)
type _ equality = ..
type (_, _) eq = Eq: ('a, 'a) eq

module type Inst = sig
  type t

  (* Extend the equality type to include this type *)
  type _ equality += Eq : t equality
end

type 'a t = (module Inst with type t = 'a)

let make: type a. unit -> a t = fun () ->
  let module Inst = struct
    type t = a
    type _ equality += Eq : t equality
  end
  in
  (module Inst)

let eq: type a b. a t -> b t -> (a, b) eq option =
  fun (module A) (module B) ->
  match A.Eq with
  | B.Eq -> Some Eq
  | _    -> None
