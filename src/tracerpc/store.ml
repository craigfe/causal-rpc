module type S = sig
  include Irmin_git.S with type Private.Sync.endpoint = Git_unix.endpoint
  val remote_of_uri: string -> Irmin.remote
end

module Make(GitImpl: Irmin_git.G)(Contents: Irmin.Contents.S) = struct
  include Irmin_unix.Git.KV (GitImpl) (Contents)
  let remote_of_uri x = remote x
end
