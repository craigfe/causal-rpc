
module Make
    (Val: Irmin.Contents.S)
    (B: Backend.S
        with type Store.key = Irmin.Path.String_list.t
         and type Store.step = string
         and module Store.Key = Irmin.Path.String_list
         and type Store.contents = Val.t Store.contents
         and type Store.branch = string): Store.JOB_QUEUE with module Store = B.Store
