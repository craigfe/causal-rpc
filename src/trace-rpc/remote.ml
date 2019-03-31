(* Phantom type 'v encodes the inner closure of the corresponding function *)
type 'v rpc = {
  name: string;
  params: Type.Boxed.t list;
}

type 'v t = 'v rpc list

let pure r = [r]
let (<*>) a b = b::a

let get_rpc_opt = function
  | [r] -> Some r
  | _ -> None

