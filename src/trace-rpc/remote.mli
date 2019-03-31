type 'v rpc = {
  name: string;
  params: Type.Boxed.t list;
}

type 'v t = 'v rpc list

val pure: 'v rpc -> 'v t
val (<*>): 'v t -> 'v rpc -> 'v t

val get_rpc_opt: 'v t -> 'v rpc option

