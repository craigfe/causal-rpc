module I = Trace_rpc.Intmap.IntPair(Trace_rpc_unix.Make)(Irmin_unix.Git.Mem.G)
include I
