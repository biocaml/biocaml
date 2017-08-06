open Core_kernel

type t = {
  source : string option;
  line : int option;
  offset : int option
}
[@@deriving sexp]

let make ?source ?line ?offset () = {source; line; offset}
let unknown = {source=None; line=None; offset=None}

let incr_line ?(n=1) t =
  let prev = match t.line with None -> 0 | Some x -> x in
  {t with line = Some (prev + n)}

let to_string t =
  sexp_of_t t |> Sexp.to_string_hum
