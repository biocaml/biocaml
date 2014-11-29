open Core.Std

module type Identifier = sig
  include Comparable
  include Sexpable with type t := t
  val to_string : t -> string
end

module Make(S : Identifier) = struct
  type t = S.t * Biocaml_range.t
  with compare, sexp

  let make s lo hi =
    let open Or_error in
    Biocaml_range.make lo hi >>= fun r ->
    Or_error.return (s, r)

  let seq = fst

  let size (_, r) = Biocaml_range.size r

  let to_string (seq, { Biocaml_range.lo ; hi }) =
    sprintf "%s:%d-%d" (S.to_string seq) lo hi
end

include Make(String)

let of_string s =
  try Scanf.sscanf s "%s@:%d-%d" make
  with Scanf.Scan_failure _ ->
    Or_error.errorf "Seq_range.of_string: invalid format %s" s
