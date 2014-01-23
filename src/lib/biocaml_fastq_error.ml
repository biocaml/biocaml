(** Fastq errors. *)
open Biocaml_internal_pervasives

(** [`sequence_and_qualities_do_not_match (pos, sequence, qualities)]
    means [sequence] and [qualities] are of different lengths. *)
type sequence_and_qualities_do_not_match = [
  `sequence_and_qualities_do_not_match of Biocaml_pos.t * string * string
] with sexp

(** [`wrong_name_line (pos, name)] means [name] doesn't start with
    '@'. *)
type wrong_name_line = [
  `wrong_name_line of Biocaml_pos.t * string
] with sexp

(** [`wrong_comment_line (pos, comment)] means [comment] doesn't start
    with '+'. *)
type wrong_comment_line = [
  `wrong_comment_line of Biocaml_pos.t * string
] with sexp

(** [`incomplete_input (pos, lines, s)] means input ended prematurely
    before a complete item could be constructed. The complete
    [lines] parsed are provided, possibly followed by a partial line
    [s] that didn't end in a newline. *)
type incomplete_input = [
  `incomplete_input of Biocaml_pos.t * string list * string option
] with sexp

(** [`cannot_convert_to_phred_score l] means the int list [l] cannot
    be converted to a phred score. *)
type cannot_convert_to_phred_score = [
  `cannot_convert_to_phred_score of int list
] with sexp

type sequence_names_mismatch = [
  `sequence_names_mismatch of string * string
] with sexp


(** The errors of the {!Transform.fasta_pair_to_fastq}. *)
type fasta_pair_to_fastq = [
| cannot_convert_to_phred_score
| sequence_names_mismatch
] with sexp

(** Parse errors. *)
type parsing = [
| sequence_and_qualities_do_not_match
| wrong_comment_line
| wrong_name_line
| incomplete_input
] with sexp

(** Union of all possible errors. *)
type t = [
| sequence_and_qualities_do_not_match
| wrong_comment_line
| wrong_name_line
| incomplete_input
| cannot_convert_to_phred_score
| sequence_names_mismatch
] with sexp


(** Transform error to a human-readable string. *)
let t_to_string (t : t) : string =
  let string_sample s n =
    let l = String.length s in
    if n >= l then s else
      String.sub s ~pos:0 ~len:n ^ "..."
  in
  match t with
  | `sequence_and_qualities_do_not_match (pos, s,q) ->
    sprintf "[%s]: sequence and qualities do not match (%d Vs %d characters)"
      (Biocaml_pos.to_string pos) String.(length s) String.(length q)
  | `wrong_comment_line (pos, line) ->
    sprintf "[%s]: wrong comment line: %S"
      (Biocaml_pos.to_string pos) (string_sample line 14)
  | `wrong_name_line (pos, line) ->
    sprintf "[%s]: wrong name line: %S"
      (Biocaml_pos.to_string pos) (string_sample line 14)
  | `incomplete_input (pos, sl, so) ->
    sprintf "[%s]: end-of-stream reached with incomplete input: %S"
      (Biocaml_pos.to_string pos)
      (String.concat ~sep:"\n" sl ^ Option.value ~default:"" so)
  | other ->
    sexp_of_t other |> Sexplib.Sexp.to_string_hum
