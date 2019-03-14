exception Timeout

(* --------------------------------------------------------------------------- *)

(** Returned if a description or an implementation cannot be created *)
exception Invalid_description of string

(* Internal errors -- these should be private *)
exception Malformed_params of string
exception Protocol_error of string
exception Internal_type_error
exception Empty_queue
