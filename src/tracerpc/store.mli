module type S = sig
  include Irmin_git.S with type step = string
                       and type key = Irmin.Path.String_list.t

  val remote_of_uri: string -> Irmin.remote
  val sleep: float -> unit Lwt.t
  val yield: unit -> unit Lwt.t
  val initialise: unit -> unit
end

module type MAKER = functor
  (GitImpl: Irmin_git.G)
  (Contents: Irmin.Contents.S) -> S
  with type contents = Contents.t
   and type branch = string

