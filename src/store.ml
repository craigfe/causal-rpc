module type S = sig
  include Irmin_git.S with type Private.Sync.endpoint = Git_unix.endpoint
  val remote: ?headers:Cohttp.Header.t -> string -> Irmin.remote
end

module Make(Contents: Irmin.Contents.S) = Irmin_unix.Git.KV
    (Irmin_unix.Git.FS.G)
    (Contents)
