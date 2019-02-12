module I = Trace_rpc.Intmap.IntPair(Trace_rpc_unix.Make)(Irmin_unix.Git.FS.G)
include I
