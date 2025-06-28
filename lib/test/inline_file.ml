(** File stored inline with base64 encoded content. *)

type t =
  { content : string
  ; prefix : string
  ; suffix : string
  }
