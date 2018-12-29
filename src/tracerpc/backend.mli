module type S = sig
  module Store: Irmin_git.S

  val make_info: ?author:string -> ('a, Format.formatter, unit, Irmin.Info.f) format4 -> 'a
  val remote_of_uri: string -> Irmin.remote
  val sleep: float -> unit Lwt.t
  val yield: unit -> unit Lwt.t
  val initialise: unit -> unit
end

module type MAKER = functor
  (GitImpl: Irmin_git.G) (Contents: Irmin.Contents.S) -> S
  with type Store.contents = Contents.t
   and type Store.step = string
   and type Store.branch = string
   and type Store.key = Irmin.Path.String_list.t
