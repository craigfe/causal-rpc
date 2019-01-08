
module Make
    (Val: Irmin.Contents.S)
    (B: Backend.S
        with type Store.key = Irmin.Path.String_list.t
         and type Store.step = string
         and module Store.Key = Irmin.Path.String_list
         and type Store.contents = Val.t Map.contents
         and type Store.branch = string): Map.JOB_QUEUE with module Store = B.Store
