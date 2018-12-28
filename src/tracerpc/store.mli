module type S = sig
  include Irmin_git.S with type Private.Sync.endpoint = Git_unix.endpoint
  val remote_of_uri: string -> Irmin.remote
end

module Make(GitImpl: Irmin_git.G)(Contents: Irmin.Contents.S): S
  with type key = string list
   and type step = string
   and type contents = Contents.t
   and type branch = string
