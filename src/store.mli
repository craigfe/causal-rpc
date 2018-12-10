module type S = sig
  include Irmin_git.S with type Private.Sync.endpoint = Git_unix.endpoint
  val remote: ?headers:Cohttp.Header.t -> string -> Irmin.remote
end

module Make(Contents: Irmin.Contents.S): S
  with type key = string list
   and type step = string
   and type contents = Contents.t
   and type branch = string
