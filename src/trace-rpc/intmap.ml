
module Int: Irmin.Contents.S with type t = int64 = struct
  type t = int64
  let t = Irmin.Type.int64

  let merge = Irmin.Merge.(option (idempotent t))
end


module O = Interface.MakeOperation(Int)
open O

let identity_op  = declare "identity" return
let increment_op = declare "increment" return
let multiply_op  = declare "multiply" Type.(int64 @-> return)
let sleep_op     = declare "sleep" Type.(float @-> return)
let complex_op   = declare "complex" Type.(int32 @-> int64 @-> string @-> unit @-> return)

module Definition = struct
  module Val = Int
  module D = Interface.Description(Int)
  open D

  type shape = ((O.Val.t -> O.Val.t) *
                ((O.Val.t -> O.Val.t) *
                 ((float -> O.Val.t -> O.Val.t) *
                  ((int64 -> O.Val.t -> O.Val.t) *
                   (int32 -> int64 -> string -> unit -> O.Val.t -> O.Val.t)))))

  let api = define D.(identity_op @ increment_op @ sleep_op @ multiply_op @ complex_op)
end

module Implementation: Interface.IMPL with type Val.t = int64 = struct
  module Val = Int
  module I = Interface.MakeImplementation(Val)
  open I

  let identity x = x
  let increment x = Int64.add Int64.one x
  let sleep f x = (Unix.sleepf f; increment x)

  let multiply = Int64.mul
  let complex i32 i64 s () = match Int64.of_string_opt s with
    | Some i -> Int64.mul (Int64.mul (Int64.mul (Int64.of_int32 i32) i64) i)
    | None -> Int64.mul Int64.minus_one

  type shape = ((O.Val.t -> O.Val.t) *
       ((O.Val.t -> O.Val.t) *
        ((float -> O.Val.t -> O.Val.t) *
         ((int64 -> O.Val.t -> O.Val.t) *
          (int32 -> int64 -> string -> unit -> O.Val.t -> O.Val.t)))))

  let api = define
      I.((identity_op, identity)
         @ (increment_op, increment)
         @ (sleep_op, sleep)
         @ (multiply_op, multiply)
         @ (complex_op, complex))
end

module IntPair (B: Backend.MAKER) (G: Irmin_git.G) = struct
  module IntMap = Map.Make(B)(G)(Definition)(Job_queue.Make)
  module IntWorker = Worker.Make(IntMap)(Implementation)
end
