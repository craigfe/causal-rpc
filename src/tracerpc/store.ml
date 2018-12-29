module type S = sig
  include Irmin_git.S with type Private.Sync.endpoint = Git_unix.endpoint
  val remote_of_uri: string -> Irmin.remote
end

module type BACKEND = functor
  (GitImpl: Irmin_git.G) (Contents: Irmin.Contents.S) -> S
  with type contents = Contents.t
   and type step = string
   and type branch = string
   and type key = Irmin.Path.String_list.t

module Make (GitImpl: Irmin_git.G) (Contents: Irmin.Contents.S) = struct
  include Irmin_unix.Git.KV (GitImpl) (Contents)
  let remote_of_uri x = remote x
end
