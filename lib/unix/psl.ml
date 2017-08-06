open Core_kernel
module Result = Biocaml_result
open CFStream

type item = {
  matches : int;
  mismatches : int;
  rep_matches : int;
  n_count : int;
  q_num_insert : int;
  q_base_insert : int;
  t_num_insert : int;
  t_base_insert : int;
  q_name : string;
  q_strand : char;
  q_size : int;
  q_start : int;
  q_end : int;
  t_name : string;
  t_strand : char option;
  t_size : int;
  t_start : int;
  t_end : int;
  block_count : int;
  block_sizes : int list;
  q_starts : int list;
  t_starts : int list;
} [@@deriving sexp]

module Error = struct
  type t = [
  | `incomplete_input of Pos.t * string list * string option
  | `invalid_int of Pos.t * string * string
  | `invalid_strands of Pos.t * string * string
  | `invalid_number_of_columns of Pos.t * string * int
  ]
end

let parse_int msg pos s =
  try Ok (int_of_string (String.strip s))
  with _ -> Error (`invalid_int (pos,msg,s))

let parse_comma_ints msg pos s =
  (* PSL files from BLAT include extra ending comment,
     so strip that too in addition to whitespace *)
  let drop = function ' ' | '\n' | '\t' | '\r' | ',' -> true | _ -> false in
  String.strip ~drop s
  |> String.split ~on:','
  |> Result.List.mapi ~f:(fun _ -> parse_int msg pos)

let parse_string s =
  Ok (String.strip s)

let parse_strands msg pos s = match String.strip s with
  | "+" -> Ok ('+', None)
  | "-" -> Ok ('-', None)
  | "++" -> Ok ('+', Some '+')
  | "+-" -> Ok ('+', Some '-')
  | "-+" -> Ok ('-', Some '+')
  | "--" -> Ok ('-', Some '-')
  | s -> Error (`invalid_strands (pos,msg,s))

let parse_line pos line =
  let open Result.Monad_infix in
  match Line.split ~on:'\t' line with
  | [matches; mismatches; rep_matches; n_count; q_num_insert; q_base_insert;
     t_num_insert; t_base_insert; strands; q_name; q_size; q_start; q_end;
     t_name; t_size; t_start; t_end; block_count; block_sizes;
     q_starts; t_starts
    ] ->
    parse_int "matches" pos matches >>= fun matches ->
    parse_int "mismatches" pos mismatches >>= fun mismatches ->
    parse_int "rep_matches" pos rep_matches >>= fun rep_matches ->
    parse_int "n_count" pos n_count >>= fun n_count ->
    parse_int "q_num_insert" pos q_num_insert >>= fun q_num_insert ->
    parse_int "q_base_insert" pos q_base_insert >>= fun q_base_insert ->
    parse_int "t_num_insert" pos t_num_insert >>= fun t_num_insert ->
    parse_int "t_base_insert" pos t_base_insert >>= fun t_base_insert ->
    parse_strands "strands" pos strands >>= fun (q_strand,t_strand) ->
    parse_string q_name >>= fun q_name ->
    parse_int "q_size" pos q_size >>= fun q_size ->
    parse_int "q_start" pos q_start >>= fun q_start ->
    parse_int "q_end" pos q_end >>= fun q_end ->
    parse_string t_name >>= fun t_name ->
    parse_int "t_size" pos t_size >>= fun t_size ->
    parse_int "t_start" pos t_start >>= fun t_start ->
    parse_int "t_end" pos t_end >>= fun t_end ->
    parse_int "block_count" pos block_count >>= fun block_count ->
    parse_comma_ints "block_sizes" pos block_sizes >>= fun block_sizes ->
    parse_comma_ints "q_starts" pos q_starts >>= fun q_starts ->
    parse_comma_ints "t_starts" pos t_starts >>= fun t_starts ->
    Ok {
      matches; mismatches; rep_matches; n_count; q_num_insert; q_base_insert;
      t_num_insert; t_base_insert; q_name; q_strand; q_size; q_start; q_end;
      t_name; t_strand; t_size; t_start; t_end; block_count; block_sizes;
      q_starts; t_starts}
  | l -> Error
    (`invalid_number_of_columns
        (pos, (line : Line.t :> string), List.length l))

let line_to_item = parse_line

let version_line = "psLayout version 3"
let is_spaces_line = String.for_all ~f:((=) ' ')
let header_line1 = "match\tmis- \trep. \tN's\tQ gap\tQ gap\tT gap\tT gap\tstrand\tQ        \tQ   \tQ    \tQ  \tT        \tT   \tT    \tT  \tblock\tblockSizes \tqStarts\t tStarts"
let header_line2 = "     \tmatch\tmatch\t   \tcount\tbases\tcount\tbases\t      \tname     \tsize\tstart\tend\tname     \tsize\tstart\tend\tcount"
let is_dashes_line = String.for_all ~f:((=) '-')

module Transform = struct

  let string_to_item ?filename () =
    let name = sprintf "psl_parser:%s" (Option.value ~default:"<>" filename) in
    Lines.Transform.make_merge_error
      ~name ?filename ~next:(fun linebuf ->
        let open Lines.Buffer in
        let rec get_line () =
          match next_line linebuf with
          | None -> `not_ready
          | Some line ->
            let line' = (line :> string) in
            if line' = version_line
            || is_spaces_line line'
            || line' = header_line1
            || line' = header_line2
            || is_dashes_line line'
            then
              get_line ()
            else
              `output (line_to_item (current_position linebuf) line)
        in
        get_line()
      ) ()

end

let in_channel_to_item_stream ?(buffer_size=65536) ?filename inp =
  Tfxm.in_channel_strings_to_stream
    ~buffer_size
    inp
    (Transform.string_to_item ?filename ())

exception Error of Error.t

let error_to_exn err = Error err

let in_channel_to_item_stream_exn ?(buffer_size=65536) ?filename inp =
  Stream.result_to_exn ~error_to_exn (
    in_channel_to_item_stream ~buffer_size ?filename inp
  )
