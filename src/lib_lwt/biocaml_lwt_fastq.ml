open Biocaml_internal_pervasives
open Biocaml
open Lwt
open Biocaml_fastq

let get_lwt_stream ic =
  let strm = Lwt_io.read_lines ic in
  Lwt_stream.from (fun () ->
    Lwt_stream.get strm >>= function
    | None -> return None
    | Some line -> (
      let name = Parse.name (Line.of_string_unsafe line) in
      Lwt_stream.get strm >>= function
      | None -> fail (
        Parse_error
          (Pos.unknown, "premature end-of-input, no sequence line")
      )
      | Some line -> (
        let sequence = Parse.sequence (Line.of_string_unsafe line) in
        Lwt_stream.get strm >>= function
        | None -> fail (
          Parse_error
            (Pos.unknown, "premature end-of-input, no comment line")
        )
        | Some line -> (
          let comment = Parse.comment (Line.of_string_unsafe line) in
          Lwt_stream.get strm >>= function
          | None -> fail (
            Parse_error
              (Pos.unknown,
               "premature end-of-input, no qualities line")
          )
          | Some line ->
            let qualities =
              Parse.qualities (Line.of_string_unsafe line)
            in
            return (Some {name; sequence; comment; qualities})
        ) ) )
  )
