
module Type: Map.QUEUE_TYPE

module Make
    (Val: Irmin.Contents.S)
    (St: Irmin_unix.Git.S
        with type key = Irmin.Path.String_list.t
         and type step = string
         and module Key = Irmin.Path.String_list
         and type contents = (Val.t, Type.t) Map.contents
         and type branch = string
         and module Git = Irmin_unix.Git.FS.G): Map.JOB_QUEUE with module Store = St
