(** User-facing errors *)

exception Timeout
(** Raised if a remote connection is inactive for longer than the specified
    timeout period *)

exception Invalid_description of string
(** Raised if a description or an implementation cannot be created *)


(**  Internal errors *)

exception Malformed_params of string
(** Raised if the parameters retrieved from the CausalRPC store do not
    match the type of the requested operation. *)

exception Protocol_error of string
(** Raised if the communication protocol between the client & server
    reaches an invalid state. *)

exception Internal_type_error
(** Raised if the value retrieved from an Irmin store does not have the
    expected type, given its location. *)

exception Empty_queue
(** Raised when attempting to pop a work item from an empty work set *)
