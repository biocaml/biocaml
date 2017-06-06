open Core_kernel.Std
open CFStream

type comment = [
| `comment of string
]
[@@deriving sexp]
type variable_step = [
| `variable_step_state_change of string * int option (* name x span *)
| `variable_step_value of int * float
]
[@@deriving sexp]
type fixed_step = [
| `fixed_step_state_change of string * int * int * int option
(* name, start, step, span *)
| `fixed_step_value of float
]
[@@deriving sexp]
type bed_graph_value = string * int * int * float
[@@deriving sexp]

type item = [comment | variable_step | fixed_step | `bed_graph_value of bed_graph_value ]
[@@deriving sexp]

(* `module_error` should progressively allow to “tag” error values. *)
let module_error e = Error (`wig e)

module Tags = struct
  type t = {
    allow_empty_lines: bool;
    sharp_comments: bool;
  }
  [@@deriving sexp]

  let default = {allow_empty_lines = false; sharp_comments = true}

  let to_string t = sexp_of_t t |> Sexplib.Sexp.to_string
  let of_string s =
    try Ok (Sexplib.Sexp.of_string s |> t_of_sexp)
    with e -> module_error (`tags_of_string e)

end

module Error = struct
  type parsing = [
    | `cannot_parse_key_values of Pos.t * string
    | `empty_line of Pos.t
    | `incomplete_input of Pos.t * string list * string option
    | `missing_chrom_value of Pos.t * string
    | `missing_start_value of Pos.t * string
    | `missing_step_value of Pos.t * string
    | `wrong_start_value of Pos.t * string
    | `wrong_step_value of Pos.t * string
    | `unrecognizable_line of Pos.t * string list
    | `wrong_bed_graph_value of Pos.t * string
    | `wrong_fixed_step_value of Pos.t * string
    | `wrong_span_value of Pos.t * string
    | `wrong_variable_step_value of Pos.t * string
  ]
  [@@deriving sexp]

  let parsing_error_to_string =
    let pos () a = Pos.to_string a in
    function
    | `cannot_parse_key_values (p, s) ->
      sprintf "cannot_parse_key_values (%a, %S)" pos p s
    | `empty_line p -> sprintf "empty_line (%a)" pos p
    | `incomplete_input (p, vs, vo) -> (* Pos.t * string list *string option *)
      sprintf "incomplete_input (%a, %s, %S)" pos p
        (String.concat ~sep:"; " vs) (Option.value ~default:"" vo)
    | `missing_chrom_value (p, v) -> (* Pos.t * string *)
      sprintf "missing_chrom_value (%a, %s)" pos p v
    | `missing_start_value (p, v) -> (* Pos.t * string *)
      sprintf "missing_start_value (%a, %s)" pos p v
    | `missing_step_value (p, v) -> (* Pos.t * string *)
      sprintf "missing_step_value (%a, %s)" pos p v
    | `wrong_start_value (p, v) -> (* Pos.t * string *)
      sprintf "wrong_start_value (%a, %s)" pos p v
    | `wrong_step_value (p, v) -> (* Pos.t * string *)
      sprintf "wrong_step_value (%a, %s)" pos p v
    | `unrecognizable_line (p, v) -> (* Pos.t * string list *)
      sprintf "unrecognizable_line (%a, %s)" pos p (String.concat ~sep:" " v)
    | `wrong_bed_graph_value (p, v) -> (* Pos.t * string *)
      sprintf "wrong_bed_graph_value (%a, %s)" pos p v
    | `wrong_fixed_step_value (p, v) -> (* Pos.t * string *)
      sprintf "wrong_fixed_step_value (%a, %s)" pos p v
    | `wrong_span_value (p, v) -> (* Pos.t * string *)
      sprintf "wrong_span_value (%a, %s)" pos p v
    | `wrong_variable_step_value (p, v) -> (* Pos.t * string *)
      sprintf "wrong_variable_step_value (%a, %s)" pos p v

  type to_bed_graph = [`not_in_variable_step_state | `not_in_fixed_step_state]
  [@@deriving sexp]

  type t = [ parsing | to_bed_graph ] [@@deriving sexp]
end

module Transform = struct
  open Result.Monad_infix

  let explode_key_value loc s =
    try
      let by_space =
        String.split_on_chars s ~on:[' '; '\n'; '\t'; '\r']
        |> List.filter ~f:((<>) "") in
      Ok (List.map by_space ~f:(fun s ->
        begin match String.split ~on:'=' s with
        | [ key; value ] -> (key, value)
        | _ -> raise Not_found
        end))
    with
      Not_found -> Error (`cannot_parse_key_values (loc, s))

  let rec next ~tags p =
    let open Lines.Buffer in
    let assoc_find ~missing l v =
      match List.Assoc.find ~equal:String.equal l v with | Some v -> Ok v | None -> Error missing in
    let assoc_find_map ~missing ~wrong ~f l v =
      match List.Assoc.find ~equal:String.equal l v with
      | Some v -> (try Ok (f v) with _ -> Error wrong)
      | None -> Error missing in
    match (next_line p :> string option) with
    | Some "" ->
      if tags.Tags.allow_empty_lines
      then `output (Error (`empty_line (current_position p)))
      else next ~tags p
    | Some l when tags.Tags.sharp_comments && String.is_prefix l ~prefix:"#" ->
      `output (Ok (`comment String.(sub l ~pos:1 ~len:(length l - 1))))
    | Some l when String.is_prefix l ~prefix:"fixedStep" ->
      let output_m =
        explode_key_value (current_position p)
          String.(chop_prefix_exn l ~prefix:"fixedStep")
        >>= fun assoc ->
        assoc_find assoc "chrom"
          ~missing:(`missing_chrom_value (current_position p, l))
        >>= fun chrom ->
        assoc_find_map assoc "start"
          ~missing:(`missing_start_value (current_position p, l))
          ~f:Int.of_string ~wrong:(`wrong_start_value (current_position p, l))
        >>= fun start ->
        assoc_find_map assoc "step"
          ~missing:(`missing_step_value (current_position p, l))
          ~f:Int.of_string ~wrong:(`wrong_step_value (current_position p, l))
        >>= fun step ->
        begin match List.Assoc.find ~equal:String.equal assoc "span" with
        | None ->
          Ok (`fixed_step_state_change (chrom, start, step, None))
        | Some span ->
          begin match Option.try_with (fun () -> Int.of_string span) with
          | Some i ->
            Ok (`fixed_step_state_change (chrom, start, step, Some i))
          | None -> Error (`wrong_span_value (current_position p, span))
          end
        end
      in
      `output output_m
    | Some l when String.is_prefix l ~prefix:"variableStep" ->
      let output_m =
        explode_key_value (current_position p)
          String.(chop_prefix_exn l ~prefix:"variableStep")
        >>= fun assoc ->
        assoc_find assoc "chrom"
          ~missing:(`missing_chrom_value (current_position p, l))
        >>= fun chrom ->
        begin match List.Assoc.find ~equal:String.equal assoc "span" with
        | None -> Ok (`variable_step_state_change (chrom, None))
        | Some span ->
          begin match Option.try_with (fun () -> Int.of_string span) with
          | Some i -> Ok (`variable_step_state_change (chrom, Some i))
          | None -> Error (`wrong_span_value (current_position p, span))
          end
        end
      in
      `output output_m
    | Some l ->
      let by_space =
        String.split_on_chars l ~on:[' '; '\n'; '\t'; '\r']
        |> List.filter ~f:((<>) "") in
      begin match by_space with
      | [ one_value ] ->
        (try `output (Ok (`fixed_step_value Float.(of_string one_value)))
         with _ -> `output (Error (`wrong_fixed_step_value (current_position p, l))))
      | [ fst_val; snd_val] ->
        (try `output (Ok (`variable_step_value (Int.of_string fst_val,
                                            Float.of_string snd_val)))
         with _ -> `output (Error (`wrong_variable_step_value (current_position p, l))))
      | [ chr; b; e; v; ] ->
        (try `output (Ok (`bed_graph_value (chr,
                                        Int.of_string b,
                                        Int.of_string e,
                                        Float.of_string v)))
         with _ -> `output (Error (`wrong_bed_graph_value (current_position p, l))))
      | l ->
        `output (Error (`unrecognizable_line (current_position p, l)))
      end
    | None ->
      `not_ready


  let string_to_item ?filename ?(tags=Tags.default) () =
    let name = sprintf "wig_parser:%s" Option.(value ~default:"<>" filename) in
    let next = next ~tags in
    Lines.Transform.make_merge_error ~name ?filename ~next ()


  let item_to_string ?(tags=Tags.default) () =
    let to_string = function
    | `comment c -> if tags.Tags.sharp_comments then sprintf "#%s\n" c else ""
    | `variable_step_state_change (chrom, span) ->
      sprintf "variableStep chrom=%s%s\n" chrom
        Option.(value_map ~default:"" span ~f:(sprintf " span=%d"))
    | `variable_step_value (pos, v) -> sprintf "%d %g\n" pos v
    | `fixed_step_state_change (chrom, start, step, span) ->
      sprintf "fixedStep chrom=%s start=%d step=%d%s\n" chrom start step
        Option.(value_map ~default:"" span ~f:(sprintf " span=%d"))
    | `fixed_step_value v -> sprintf "%g\n" v
    | `bed_graph_value (chrom, start, stop, v) ->
      sprintf "%s %d %d %g\n" chrom start stop v in
    Tfxm.of_function ~name:"wig_to_string" to_string

  let item_to_bed_graph () =
    let queue = Queue.create () in
    let current_state = ref None in
    Tfxm.make ~name:"wig_to_variable_step" ()
      ~feed:(function
      | `comment _ -> ()
      | `bed_graph_value already_done ->
        Queue.enqueue queue (`output (Ok already_done))
      | `variable_step_state_change (chrom, span) ->
        current_state := Some (`variable (chrom, span))
      | `variable_step_value (pos, v) ->
        begin match !current_state with
        | Some (`variable (chrom, span)) ->
          let stop = pos + Option.(value ~default:1 span) - 1 in
          Queue.enqueue queue (`output (Ok  (chrom, pos, stop, v)))
        | _ ->
          Queue.enqueue queue (`output (Error (`not_in_variable_step_state)))
        end
      | `fixed_step_state_change (chrom, start, step, span) ->
        current_state := Some (`fixed (chrom, start, step , span, 0))
      | `fixed_step_value v ->
        begin match !current_state with
        | Some (`fixed (chrom, start, step, span, current)) ->
          let pos = start + (step * current) in
          let stop = pos + Option.(value ~default:1 span) - 1 in
          Queue.enqueue queue (`output (Ok (chrom, pos, stop, v)));
          current_state := Some  (`fixed (chrom, start, step , span, current + 1))
        | _ ->
          Queue.enqueue queue (`output (Error (`not_in_fixed_step_state)))
        end)
      ~next:(fun stopped ->
        match Queue.dequeue queue with
        | None -> if stopped then `end_of_stream else `not_ready
        | Some v -> v)

end

exception Error of  Error.t
let error_to_exn e = Error e

let in_channel_to_item_stream ?(buffer_size=65536) ?filename ?tags inp =
  let x = Transform.string_to_item ?filename ?tags () in
  Tfxm.(in_channel_strings_to_stream inp x ~buffer_size)

let in_channel_to_item_stream_exn ?buffer_size ?filename ?tags inp =
  Stream.result_to_exn ~error_to_exn
    (in_channel_to_item_stream ?filename ?buffer_size ?tags inp)

let in_channel_to_bed_graph ?(buffer_size=65536) ?filename ?tags inp =
  let x = Transform.string_to_item ?filename ?tags () in
  let y = Transform.item_to_bed_graph () in
  Tfxm.(
    compose_results x y ~on_error:(function `left x -> x | `right x -> x)
    |> in_channel_strings_to_stream ~buffer_size inp
  )

let in_channel_to_bed_graph_exn ?buffer_size ?filename ?tags inp =
  Stream.result_to_exn ~error_to_exn
    (in_channel_to_bed_graph ?filename ?buffer_size ?tags inp)


let item_to_string ?(tags=Tags.default) =
  function
  | `comment c -> if tags.Tags.sharp_comments then sprintf "#%s\n" c else ""
  | `variable_step_state_change (chrom, span) ->
    sprintf "variableStep chrom=%s%s\n" chrom
      Option.(value_map ~default:"" span ~f:(sprintf " span=%d"))
  | `variable_step_value (pos, v) -> sprintf "%d %g\n" pos v
  | `fixed_step_state_change (chrom, start, step, span) ->
    sprintf "fixedStep chrom=%s start=%d step=%d%s\n" chrom start step
      Option.(value_map ~default:"" span ~f:(sprintf " span=%d"))
  | `fixed_step_value v -> sprintf "%g\n" v
  | `bed_graph_value (chrom, start, stop, v) ->
    sprintf "%s %d %d %g\n" chrom start stop v
