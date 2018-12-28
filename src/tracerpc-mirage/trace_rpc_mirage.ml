module Info: Trace_rpc.Info.S = struct
  let make = Irmin_mirage.info
end

module Make(GitImpl: Irmin_git.G)(Contents: Irmin.Contents.S) = Irmin_mirage.Git.KV
    (GitImpl)
    (Contents)
