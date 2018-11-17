
module Type: Map.QUEUE_TYPE

module Make
    (Val: Irmin.Contents.S)
    (St: Irmin.KV with type contents = (Val.t, Type.t) Map.contents): Map.JOB_QUEUE with module Store = St
