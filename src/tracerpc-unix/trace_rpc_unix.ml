module Info: Trace_rpc.Info.S = struct
  let make = Irmin_unix.info
end

module Make (GitImpl: Irmin_git.G) (Contents: Irmin.Contents.S): Trace_rpc.Store.S
  with type contents = Contents.t
   and type branch = string
= struct
  include Irmin_unix.Git.FS.KV (Contents)

  let remote_of_uri (x:string) = remote ?headers:None x
  let sleep = Lwt_unix.sleep
  let yield = Lwt_main.yield
  let initialise = Irmin_unix.set_listen_dir_hook
end
