open Biocaml_internal_pervasives
open Result
module Pos = Biocaml_pos
module Line = Biocaml_line

let dbg = Debug.make "fastq"

type item = {
  name: string;
  sequence: string;
  comment: string;
  qualities: string;
}
with sexp

module Error = struct

  type fasta_pair_to_fastq =
    [ `cannot_convert_to_phred_score of int list
    | `sequence_names_mismatch of string * string ]
  with sexp

  type parsing =
      [ `sequence_and_qualities_do_not_match of Biocaml_pos.t * string * string
      | `wrong_comment_line of Biocaml_pos.t * string
      | `wrong_name_line of Biocaml_pos.t * string
      | `incomplete_input of Biocaml_pos.t * string list * string option
      ]
  with sexp

  type t = [ parsing | fasta_pair_to_fastq ]
  with sexp

  let string_sample s n =
    let l = String.length s in
    if n >= l then s else
      String.sub s ~pos:0 ~len:n ^ "..."

  let t_to_string = function
    | `sequence_and_qualities_do_not_match (pos, s,q) ->
        sprintf "[%s]: sequence and qualities do not match (%d Vs %d characters)"
          (Pos.to_string pos) String.(length s) String.(length q)
    | `wrong_comment_line (pos, line) ->
        sprintf "[%s]: wrong comment line: %S"
          (Pos.to_string pos) (string_sample line 14)
    | `wrong_name_line (pos, line) ->
        sprintf "[%s]: wrong name line: %S"
          (Pos.to_string pos) (string_sample line 14)
    | `incomplete_input (pos, sl, so) ->
        sprintf "[%s]: end-of-stream reached with incomplete input: %S"
          (Pos.to_string pos)
          (String.concat ~sep:"\n" sl ^ Option.value ~default:"" so)
    | other ->
      sexp_of_t other |> Sexplib.Sexp.to_string_hum

end

exception Parse_error of Biocaml_pos.t * string

let name_of_line ?(pos=Pos.unknown) line =
  let line = (line : Line.t :> string) in
  let n = String.length line in
  if n = 0 || line.[0] <> '@' then
    raise (Parse_error (pos, "name line must begin with '@'"))
  else
    String.sub line ~pos:1 ~len:(n-1)

let sequence_of_line ?(pos=Pos.unknown) line =
  (line : Line.t :> string)

let comment_of_line ?(pos=Pos.unknown) line =
  let line = (line : Line.t :> string) in
  let n = String.length line in
  if n = 0 || line.[0] <> '+' then
    raise (Parse_error (pos, "comment line must begin with '+'"))
  else
    String.sub line ~pos:1 ~len:(n-1)

let qualities_of_line ?(pos=Pos.unknown) ?sequence line =
  let line = (line : Line.t :> string) in
  match sequence with
  | None -> line
  | Some sequence ->
    let m = String.length sequence in
    let n = String.length line in
    if m <> n then
      raise (Parse_error (
        pos,
        sprintf "got %d quality scores for %d sequence items" m n
      ))
    else
      line


module MakeIO (Future : Future.S) = struct
  open Future

  let read ic =
    let read_one ic =
      Reader.read_line ic >>= function
      | `Eof -> return `Eof
      | `Ok line -> (
        let name = name_of_line (Line.of_string_unsafe line) in
        Reader.read_line ic >>= function
        | `Eof -> fail (
          Parse_error
            (Pos.unknown, "premature end-of-input, no sequence line")
        )
        | `Ok line -> (
          let sequence = sequence_of_line (Line.of_string_unsafe line) in
          Reader.read_line ic >>= function
          | `Eof -> fail (
            Parse_error
              (Pos.unknown, "premature end-of-input, no comment line")
          )
          | `Ok line -> (
            let comment = comment_of_line (Line.of_string_unsafe line) in
            Reader.read_line ic >>= function
            | `Eof -> fail (
              Parse_error
                (Pos.unknown, "premature end-of-input, no qualities line")
            )
            | `Ok line ->
              let qualities = qualities_of_line (Line.of_string_unsafe line) in
              return (`Ok {name; sequence; comment; qualities})
          ) ) )
    in
    Reader.read_all ic read_one

end

include MakeIO(Future_std)

module Transform = struct
  let string_to_item ?filename () =
    let name = sprintf "fastq_parser:%s" Option.(value ~default:"<>" filename) in
    Biocaml_lines.Transform.make_merge_error
      ~name ?filename ~next:(fun p ->
        let open Biocaml_lines.Buffer in
        if queued_lines p < 4 then
          `not_ready
        else (
          let name_line  = (next_line_exn p :> string) in
          if String.length name_line = 0 || name_line.[0] <> '@'
          then output_error (`wrong_name_line (current_position p, name_line))
          else
            let sequence     = (next_line_exn p :> string) in
            let comment_line = (next_line_exn p :> string) in
            if String.length comment_line = 0 || comment_line.[0] <> '+'
            then output_error (`wrong_comment_line (current_position p, comment_line))
            else
              let qualities    = (next_line_exn p :> string) in
              if String.length sequence <> String.length qualities
              then output_error
                (`sequence_and_qualities_do_not_match (current_position p,
                                                      sequence, qualities))
              else (
                output_ok {
                  name = String.sub name_line 1 (String.length name_line - 1);
                  comment = String.sub comment_line 1 (String.length comment_line - 1);
                  sequence; qualities }
              ))
      ) ()

  let item_to_string_pure r =
    sprintf "@%s\n%s\n+%s\n%s\n" r.name r.sequence r.comment r.qualities

  let item_to_string () =
    Biocaml_transform.of_function ~name:"fastq_to_string" item_to_string_pure

  let trim (specification: [`beginning of int|`ending of int]) =
    let items =  Queue.create () in
    let name =
      sprintf "(fastq_trimmer %s)"
        (match specification with
        | `beginning i -> sprintf "B:%d" i
        | `ending i -> sprintf "E:%d" i) in
    Biocaml_transform.make ~name ()
      ~feed:(fun r -> Queue.enqueue items r)
      ~next:(fun stopped ->
        begin match Queue.dequeue items with
        | Some r ->
          let rlgth = String.length r.sequence in
          begin match specification with
          | `beginning i when i < rlgth ->
            output_ok
              { r with sequence = String.sub r.sequence ~pos:i ~len:(rlgth - i);
                qualities = String.sub r.qualities ~pos:i ~len:(rlgth - i) }
          | `ending i when i < rlgth ->
            output_ok
              { r with sequence = String.sub r.sequence ~pos:0 ~len:(rlgth - i);
                qualities = String.sub r.qualities ~pos:0 ~len:(rlgth - i) }
          | _ ->
            output_error (`invalid_size rlgth)
          end
        | None -> if stopped then `end_of_stream else `not_ready
        end)


  let fasta_pair_to_fastq ?(phred_score_offset=`offset33) () =
    let open Result in
    let module Fasta = Biocaml_fasta in
    Biocaml_transform.of_function begin fun (char_item, int_item) ->
      if char_item.Fasta.header = int_item.Fasta.header then
        begin
          begin try
            List.map int_item.Fasta.sequence (fun int ->
                Biocaml_phred_score.(
                  of_int_exn int
                  |> to_ascii_exn ~offset:phred_score_offset
                  |> Char.to_string))
            |> String.concat ~sep:"" |> return
          with _ ->
            fail (`cannot_convert_to_phred_score int_item.Fasta.sequence)
          end
          >>= fun qualities ->
          return {name = char_item.Fasta.header;
                  sequence = char_item.Fasta.sequence;
                  comment = char_item.Fasta.header;
                  qualities}
        end
      else
        fail (`sequence_names_mismatch (char_item.Fasta.header,
                                        int_item.Fasta.header))
    end

  let fastq_to_fasta_pair  ?(phred_score_offset=`offset33) () =
    let open Result in
    Biocaml_transform.of_function begin fun {name; sequence; qualities; _} ->
      begin try
        let scores =
          String.fold ~init:[] qualities ~f:(fun prev c ->
              Biocaml_phred_score.(
                of_ascii_exn ~offset:phred_score_offset c |> to_int) :: prev)
          |> List.rev in
        return Biocaml_fasta.({ header = name; sequence },
                              { header = name; sequence = scores })
      with e -> (* exception from the Phred-score convertions *)
        fail (`cannot_convert_ascii_phred_score qualities)
      end
    end


end

let in_channel_to_item_stream ?(buffer_size=65536) ?filename inp =
  Transform.string_to_item ?filename ()
  |! Biocaml_transform.in_channel_strings_to_stream ~buffer_size inp

exception Error of Error.t

let error_to_exn err = Error err

let in_channel_to_item_stream_exn ?(buffer_size=65536) ?filename inp =
  Stream.result_to_exn ~error_to_exn (
    in_channel_to_item_stream ~buffer_size ?filename inp
  )

let item_to_string = Transform.item_to_string_pure
