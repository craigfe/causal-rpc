open Mirage

let address =
  let network = Ipaddr.V4.Prefix.of_address_string_exn "10.0.42.2/24"
  and gateway = Ipaddr.V4.of_string "10.0.42.1"
  in
  { network ; gateway }

let net =
  if_impl Key.is_unix
    (socket_stackv4 [Ipaddr.V4.any])
    (static_ipv4_stack ~config:address ~arp:farp default_network)

let key =
  let doc = Key.Arg.info ~doc:"The remote URI of the client" ["client"] in
  Key.(create "client" Arg.(opt string "git://github.com/CraigFe/trace-rpc-example.git" doc))

let main =
  let packages = [
    package "duration";
    package "trace-rpc";
    package "trace-rpc-mirage";
    package "irmin";
    package "irmin-mirage";
  ] in

  foreign
    ~deps:[abstract nocrypto]
    ~keys:[Key.abstract key]
    ~packages
    "Unikernel.Main"
    (time @-> pclock @-> resolver @-> conduit @-> job)

let () =
  register "tracerpc-worker" [main $ default_time $ default_posix_clock $
                              resolver_dns net $ conduit_direct net]
