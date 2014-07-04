open Core.Std
open Biocaml_internal_utils
module Lines = Biocaml_lines

type item = {
  name: string;
  sequence: string;
  comment: string;
  qualities: string;
} with sexp


module Err = Biocaml_fastq_error
exception Parse_error of Err.parsing
exception Err of Err.t


(******************************************************************************)
(* Printing                                                                   *)
(******************************************************************************)
let item_to_string r =
  sprintf "@%s\n%s\n+%s\n%s\n" r.name r.sequence r.comment r.qualities


(******************************************************************************)
(* Parsing                                                                    *)
(******************************************************************************)
let name_of_line ?(pos=Pos.unknown) line =
  let line = (line : Line.t :> string) in
  let n = String.length line in
  if n = 0 || line.[0] <> '@' then
    error
      "invalid name"
      (pos, line)
      <:sexp_of< Pos.t * string >>
  else
    Ok (String.sub line ~pos:1 ~len:(n-1))

let sequence_of_line ?(pos=Pos.unknown) line =
  (line : Line.t :> string)

let comment_of_line ?(pos=Pos.unknown) line =
  let line = (line : Line.t :> string) in
  let n = String.length line in
  if n = 0 || line.[0] <> '+' then
    error
      "invalid comment"
      (pos, line)
      <:sexp_of< Pos.t * string >>
  else
    Ok (String.sub line ~pos:1 ~len:(n-1))

let qualities_of_line ?(pos=Pos.unknown) ?sequence line =
  let line = (line : Line.t :> string) in
  match sequence with
  | None -> Ok line
  | Some sequence ->
    let m = String.length sequence in
    let n = String.length line in
    if m <> n then
      error
        "length of sequence and qualities differ"
        (pos, sequence, line)
        <:sexp_of< Pos.t * string * string >>
    else
      Ok line


(******************************************************************************)
(* Transforms                                                                 *)
(******************************************************************************)
module Transform = struct
  let string_to_item ?filename () =
    let name = sprintf "fastq_parser:%s" Option.(value ~default:"<>" filename) in
    Lines.Transform.make_merge_error
      ~name ?filename ~next:(fun p ->
        let open Lines.Buffer in
        if queued_lines p < 4 then
          `not_ready
        else (
          let name_line  = (next_line_exn p :> string) in
          if String.length name_line = 0 || name_line.[0] <> '@'
          then `output (Error (`invalid_name (current_position p, name_line)))
          else
            let sequence     = (next_line_exn p :> string) in
            let comment_line = (next_line_exn p :> string) in
            if String.length comment_line = 0 || comment_line.[0] <> '+'
            then `output (Error (`invalid_comment (current_position p, comment_line)))
            else
              let qualities    = (next_line_exn p :> string) in
              if String.length sequence <> String.length qualities
              then `output (Error
                (`sequence_qualities_mismatch (current_position p,
                                                      sequence, qualities)))
              else (
                `output (Ok {
                  name = String.sub name_line 1 (String.length name_line - 1);
                  comment = String.sub comment_line 1 (String.length comment_line - 1);
                  sequence; qualities })
              ))
      ) ()

  let item_to_string () =
    Biocaml_transform.of_function ~name:"fastq_to_string" item_to_string

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
            `output (Ok
              { r with sequence = String.sub r.sequence ~pos:i ~len:(rlgth - i);
                qualities = String.sub r.qualities ~pos:i ~len:(rlgth - i) })
          | `ending i when i < rlgth ->
            `output (Ok
              { r with sequence = String.sub r.sequence ~pos:0 ~len:(rlgth - i);
                qualities = String.sub r.qualities ~pos:0 ~len:(rlgth - i) })
          | _ ->
            `output (Error (`invalid_size rlgth))
          end
        | None -> if stopped then `end_of_stream else `not_ready
        end)


  let fasta_pair_to_fastq ?(phred_score_offset=`Offset33) () =
    let open Result.Monad_infix in
    let module Fasta = Biocaml_fasta in
    Biocaml_transform.of_function begin fun (char_item, int_item) ->
      if char_item.Fasta.header = int_item.Fasta.header then
        begin
          begin try
            List.map int_item.Fasta.sequence (fun int ->
                Biocaml_phred_score.(
                  ok_exn (of_int int)
                  |> fun x -> ok_exn (to_ascii ~offset:phred_score_offset x)
                  |> Char.to_string))
            |> String.concat ~sep:"" |> Result.return
          with _ ->
            Error (`cannot_convert_to_phred_score int_item.Fasta.sequence)
          end
          >>= fun qualities ->
          Ok {name = char_item.Fasta.header;
                  sequence = char_item.Fasta.sequence;
                  comment = char_item.Fasta.header;
                  qualities}
        end
      else
        Error (`sequence_names_mismatch (char_item.Fasta.header,
                                        int_item.Fasta.header))
    end

  let fastq_to_fasta_pair  ?(phred_score_offset=`Offset33) () =
    Biocaml_transform.of_function begin fun {name; sequence; qualities; _} ->
      begin try
        let scores =
          String.fold ~init:[] qualities ~f:(fun prev c ->
              Biocaml_phred_score.(
                of_ascii ~offset:phred_score_offset c |> ok_exn |> to_int) :: prev)
          |> List.rev in
        Ok Biocaml_fasta.({ header = name; sequence },
                              { header = name; sequence = scores })
      with e -> (* exception from the Phred-score convertions *)
        Error (`cannot_convert_ascii_phred_score qualities)
      end
    end

end


(******************************************************************************)
(* Input/Output                                                               *)
(******************************************************************************)
let in_channel_to_item_stream ?(buffer_size=65536) ?filename inp =
  Transform.string_to_item ?filename ()
  |> Biocaml_transform.in_channel_strings_to_stream ~buffer_size inp

let error_to_exn err = Err err

let in_channel_to_item_stream_exn ?(buffer_size=65536) ?filename inp =
  Stream.result_to_exn ~error_to_exn (
    in_channel_to_item_stream ~buffer_size ?filename inp
  )

module MakeIO (Future : Future.S) = struct
  open Future

  let read_item ic : item Or_error.t Reader.Read_result.t Deferred.t =
    Reader.read_line ic >>= function
    | `Eof -> return `Eof
    | `Ok line ->
      match name_of_line (Line.of_string_unsafe line) with
      | Error _ as e -> return (`Ok e)
      | Ok name ->
        Reader.read_line ic >>= function
        | `Eof ->
          return (`Ok (Error (Error.of_string "incomplete input")))
        | `Ok line ->
          let sequence = sequence_of_line (Line.of_string_unsafe line) in
          Reader.read_line ic >>= function
          | `Eof ->
            return (`Ok (Error (Error.of_string "incomplete input")))
          | `Ok line ->
            match comment_of_line (Line.of_string_unsafe line) with
            | Error _ as e -> return (`Ok e)
            | Ok comment ->
              Reader.read_line ic >>= function
              | `Eof ->
                return (`Ok (Error (Error.of_string "incomplete input")))
              | `Ok line ->
                match
                  qualities_of_line ~sequence (Line.of_string_unsafe line)
                with
                | Error _ as e -> return (`Ok e)
                | Ok qualities ->
                  return (`Ok (Ok {name; sequence; comment; qualities}))

  let read ic =
    Reader.read_all ic read_item

  let read_file ?buf_len file =
    Reader.open_file ?buf_len file >>| read

  let write_item (w : Writer.t) (x : item) : unit Deferred.t =
    let open Writer in
    write_char w '@' >>= fun () ->
    write_line w x.name >>= fun () ->
    write_line w x.sequence >>= fun () ->
    write_char w '+' >>= fun () ->
    write_line w x.comment >>= fun () ->
    write_line w x.qualities

  let write w pipe_r =
    Pipe.iter pipe_r ~f:(write_item w)

  let write_file ?perm ?append file pipe_r =
    Writer.with_file ?perm ?append file ~f:(fun w -> write w pipe_r)

end
include MakeIO(Future_std)
