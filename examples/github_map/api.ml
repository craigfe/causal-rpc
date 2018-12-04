open Trace_rpc

type contents = string * int

module C: Irmin.Contents.S with type t = contents = struct
  type t = contents
  let t = Irmin.Type.(pair string int)
  let merge = Irmin.Merge.(option (default t))
end

module O = Interface.MakeOperation(C)
open O

let commit_count_op = declare "commit_count" return
module Definition = struct
  module Val = C
  module D = Interface.Description(C)
  open D

  let api = define [describe commit_count_op]
end

module Implementation: Interface.IMPL with type Val.t = contents = struct
  module Val = C
  module I = Interface.MakeImplementation(C)
  open I

  let api = define [implement commit_count_op (fun (s, _) -> ("", Github.commit_count s))]
end

module GithubMap = Map.Make(Definition)(Job_queue.Type)(Job_queue.Make)
module GithubWorker = Worker.Make(GithubMap)(Implementation)
