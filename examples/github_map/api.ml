open Trace_rpc

type contents = string * int

module C: Irmin.Contents.S with type t = contents = struct
  type t = contents
  let t = Irmin.Type.(pair string int)
  let merge = Irmin.Merge.(option (default t))
end

module O = Operation.Make(C)
open O

let commit_count_op = declare "commit_count" return
module Definition = struct
  module Val = C
  module D = Description.Make(C)
  open D

  type shape = contents -> contents
  let api = define (finally commit_count_op)
end

module Implementation: Interface.IMPL with type Val.t = contents = struct
  module Val = C
  module I = Interface.MakeImplementation(C)
  open I

  type shape = contents -> contents
  let api = define @@ finally (commit_count_op, (fun (s, _) -> ("", Github.commit_count s)))
end

module GithubMap = Map.Make(Store.Make(Trace_rpc_unix.Make)(Irmin_unix.Git.Mem.G)(Definition))(Implementation)
module GithubWorker = Worker.Make(GithubMap)(Implementation)
