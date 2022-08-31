open Core.Std
open Flow
open Biocaml_app_common

module Http_method = struct
  type t = string -> (string, string) Result.t Lwt.t

  let detect_exe exe =
    wrap_deferred_lwt (fun () ->
      Lwt_unix.system ("which \"" ^ exe ^ "\" > /dev/null 2>&1"))
    >>= fun ps ->
    match ps with
    | Lwt_unix.WEXITED 0 -> return true
    | _ -> return false
  ;;

  let shell_command_to_string s =
    Say.dbg "Running %S" s
    >>= fun () ->
    System.Shell.execute s
    >>= function
    | stdo, stde, `exited 0 -> return stdo
    | e -> error (`failure (sprintf "command %S failed" s))
  ;;

  let discover () =
    detect_exe "curl"
    >>= fun curls_there ->
    if curls_there
    then
      return (fun s ->
        let cmd = sprintf "curl -f -k -L %S" s in
        shell_command_to_string cmd)
    else
      detect_exe "wget"
      >>= fun wgets_there ->
      if wgets_there
      then
        return (fun s ->
          let cmd = sprintf "wget --no-check-certificate -q -O - %S" s in
          shell_command_to_string cmd)
      else failf "No HTTP command found"
  ;;
end

(* The Entrez functor expects an 'a t and cannot deal with any
   ('a, 'b) t, so we drop back to Lwt instead of Flow. *)

open Lwt

exception
  Fetch_error of
    [ `failure of string | `lwt_exn of exn | `shell of string * [ `exn of exn ] ]

let unresult m =
  m
  >>= function
  | Ok o -> return o
  | Error e -> fail (Fetch_error e)
;;

module Fetch = struct
  type 'a fetched = 'a Lwt.t

  let fetch url f =
    unresult (Http_method.discover ())
    >>= fun http -> unresult (http url) >>= fun (got : string) -> return (f got)
  ;;

  let ( >>= ) = ( >>= )
  let ( >|= ) = ( >|= )
end

module Entrez = Biocaml_entrez.Make (Fetch)

let pubmed s =
  Entrez.PubmedSummary.search (String.concat ~sep:" " s)
  >>= fun result ->
  Lwt_io.printf "Result:\n"
  >>= fun () ->
  Lwt_list.iter_s
    (fun { Entrez.PubmedSummary.pmid; title; doi } ->
      Lwt_io.printf
        "* ID: %d\n\tTitle: %s\n%s"
        pmid
        title
        Option.(value_map doi ~default:"" ~f:(sprintf "\tDOI: %s\n")))
    result
;;

let gene s =
  let open Entrez in
  Gene.search (String.concat ~sep:" " s)
  >>= fun result ->
  Lwt_io.printf "Result:\n"
  >>= fun () ->
  Lwt_list.iter_s
    (fun { Gene.summary; gene } ->
      Lwt_io.printf
        "* Gene: %s\n\tIdentifiers: %s\n%s"
        Option.(value gene.Gene_ref.locus ~default:"")
        (String.concat
           ~sep:", "
           (List.map gene.Gene_ref.db (fun db ->
              sprintf "%s:%s" db.Dbtag.db (Object_id.to_string db.Dbtag.tag))))
        Option.(value_map summary ~default:"" ~f:(sprintf "\tSummary: %s\n")))
    result
;;

let command =
  Command_line.(
    group
      ~summary:"Query the Entrez/EUtils database"
      [ ( "pubmed"
        , basic
            ~summary:"Test a simple query in pubmed journals"
            Spec.(
              verbosity_flags () +> anon (sequence ("SEARCH" %: string)) ++ uses_lwt ())
            (fun l -> pubmed l >>= fun o -> return (Ok o)) )
      ; ( "gene"
        , basic
            ~summary:"Test a simple query in Entrez Gene"
            Spec.(
              verbosity_flags () +> anon (sequence ("SEARCH" %: string)) ++ uses_lwt ())
            (fun l -> gene l >>= fun o -> return (Ok o)) )
      ])
;;
