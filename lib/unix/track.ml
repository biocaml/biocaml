type t =
  [ `track of (string * string) list
  | `comment of string
  | `browser of
    [ `position of string * int * int | `hide of [ `all ] | `unknown of string ]
  ]

type 'a content = [ `content of 'a ]
type track = t

module Error = struct
  type parsing =
    [ `incomplete_input of Pos.t * string list * string option
    | `wrong_browser_position of Pos.t * string
    | `wrong_key_value_format of (string * string) list * string * string
    ]
  [@@deriving sexp]

  type t = parsing [@@deriving sexp]
end

module Transform = struct
  (*
    browser position chr19:49304200-49310700
    browser hide all
  *)
  let parse_chormpos position s =
    try
      match String.rindex s ':' with
      | Some colon -> (
        let name = String.slice s 0 colon in
        match String.rindex s '-' with
        | Some dash ->
          let start = String.slice s (colon + 1) dash |> Int.of_string in
          let stop = String.slice s (dash + 1) (String.length s) |> Int.of_string in
          Ok (`browser (`position (name, start, stop)))
        | None -> failwith "A")
      | None -> failwith "B"
    with
    | _ -> Error (`wrong_browser_position (position, s))
  ;;

  let parse_browser position line =
    let tokens =
      String.chop_prefix ~prefix:"browser " line
      |> Option.value ~default:""
      |> String.split_on_chars ~on:[ ' '; '\t'; '\r' ]
      |> List.filter ~f:String.(( <> ) "")
    in
    match tokens with
    | [ "position"; pos ] -> parse_chormpos position pos
    | [ "hide"; "all" ] -> Ok (`browser (`hide `all))
    | _ -> Ok (`browser (`unknown line))
  ;;

  (** Parse a string potentially escaped with OCaml string
      conventions, or stop at [stop_before] character if it is not
      escaped.  Examples: {[
      (* Does not stop: *)
      escapable_string ~stop_before:\['='; '@'\]  "sdf\tsd\000 sdf fdsaf";;
      = ("sdf\tsd\000 sdf fdsaf", None, "")
      (* Reads an escaped string; *)
      escapable_string ~stop_before:\['='; '@'\]  "\"sdf\\tsd\\000\" sdf fdsaf";;
      = ("sdf\tsd\000", None, " sdf fdsa")
      escapable_string ~stop_before:\['='; '@'\]  "\"sdf\\tsd\\000\" s=df \@fdsaf";;
      = ("sdf\tsd\000", None, " s=df \@fdsa")
      escapable_string ~stop_before:\['='; '@'\]  "\"sdf\\tsd\\000\"\@ s=df \@fdsaf";;
      = ("sdf\tsd\000", Some '\@', " s=df \@fdsa")
      (* Stops at '=' or '\@' *)
      escapable_string ~stop_before:\['='; '@'\]  "sdf\tsd\000 s=df \@fdsaf";;
      = ("sdf\tsd\000 s", Some '=', "df \@fdsa")
      escapable_string ~stop_before:\['='; '@'\]  "sdf\tsd\000 sdf \@fdsaf";;
      = ("sdf\tsd\000 sdf ", Some '\@', "fdsa")
      ]} *)
  let escapable_string (s : string) ~(stop_before : char list)
    : string * char option * string
    =
    let try_escaped s =
      try Some (Scanf.sscanf s "%S%n" (fun s n -> s, n)) with
      | _ -> None
    in
    let lgth_s = String.length s in
    match try_escaped s with
    | Some (found, chars_read) ->
      if chars_read < lgth_s
      then
        if List.exists stop_before ~f:Char.(( = ) s.[chars_read])
        then found, Some s.[chars_read], String.slice s (chars_read + 1) (String.length s)
        else found, None, String.slice s chars_read (String.length s)
      else found, None, ""
    | None -> (
      match String.lfindi s ~f:(fun _ c -> List.exists stop_before ~f:Char.(( = ) c)) with
      | Some idx ->
        ( String.sub s ~pos:0 ~len:idx
        , Some s.[idx]
        , String.slice s (idx + 1) (String.length s) )
      | None -> s, None, "")
  ;;

  let parse_track stripped =
    let open Result.Monad_infix in
    let rec loop s acc =
      match escapable_string s ~stop_before:[ '=' ] with
      | tag, Some '=', rest -> (
        match escapable_string rest ~stop_before:[ ' '; '\t' ] with
        | value, _, rest ->
          let str = String.strip rest in
          if String.(str = "")
          then Ok ((tag, value) :: acc)
          else loop str ((tag, value) :: acc))
      | str, _, rest -> Error (`wrong_key_value_format (List.rev acc, str, rest))
    in
    loop stripped [] >>= fun kv -> Ok (`track (List.rev kv))
  ;;

  let rec next p =
    let open Lines.Buffer in
    match (next_line p :> string option) with
    | None -> `not_ready
    | Some "" -> next p
    | Some l when String.(is_prefix (strip l) ~prefix:"#") ->
      `output (Ok (`comment String.(sub l ~pos:1 ~len:(length l - 1))))
    | Some l when String.(strip l = "track") -> `output (Ok (`track []))
    | Some l when String.(strip l = "browser") -> `output (Ok (`browser (`unknown l)))
    | Some l when String.(is_prefix (strip l) ~prefix:"track ") ->
      parse_track (String.chop_prefix_exn ~prefix:"track " l |> String.strip)
      |> fun x -> `output x
    | Some l when String.(is_prefix (strip l) ~prefix:"browser ") ->
      parse_browser (current_position p) l |> fun x -> `output x
    | Some l -> `output (Ok (`content l))
  ;;

  let string_to_string_content ?filename () =
    let name = sprintf "track_parser:%s" Option.(value ~default:"<>" filename) in
    Lines.Transform.make_merge_error ~name ?filename ~next ()
  ;;

  let needs_escaping s =
    String.exists s ~f:(function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> false
      | _ -> true)
  ;;

  let potentially_escape s = if needs_escaping s then sprintf "%S" s else s

  let string_content_to_string ?(add_content_new_line = true) () =
    let to_string = function
      | `comment c -> sprintf "#%s\n" c
      | `track l ->
        sprintf
          "track %s\n"
          (List.map l ~f:(fun (k, v) ->
             sprintf "%s=%s" (potentially_escape k) (potentially_escape v))
          |> String.concat ~sep:" ")
      | `browser (`hide `all) -> "browser hide all\n"
      | `browser (`position (n, s, e)) -> sprintf "browser position %s:%d-%d\n" n s e
      | `browser (`unknown s) -> sprintf "browser %s\n" s
      | `content s -> if add_content_new_line then s ^ "\n" else s
    in
    Tfxm.of_function ~name:"track_to_string" to_string
  ;;

  let embed_parser ?filename =
    let track_parser = string_to_string_content ?filename () in
    Tfxm.filter_compose track_parser ~destruct:(function
      | Ok (`content s) -> `transform (s ^ "\n")
      | (Ok (`track _) | Ok (`browser _) | Ok (`comment _) | Error _) as n -> `bypass n)
  ;;

  type wig_parser_error =
    [ Error.parsing
    | Wig.Error.parsing
    ]

  type wig_t =
    [ track
    | Wig.item
    ]

  let string_to_wig ?filename () =
    let wig_parser = Wig.Transform.string_to_item ?filename () in
    embed_parser
      ?filename
      (*
    let track_parser = string_to_string_content ?filename () in
    Tfxm.filter_compose
      track_parser
      ~destruct:(function
      | Ok (`content s) -> `Yes (s ^ "\n")
      | Ok (`track _) | Ok (`browser _) | Ok (`comment _)
      | Error _ as n -> `No n) *)
      wig_parser
      ~reconstruct:(function
      | `bypassed (Ok f) -> Ok (f :> wig_t)
      | `bypassed (Error f) -> Error (f :> [> wig_parser_error ])
      | `transformed (Ok o) -> Ok (o :> wig_t)
      | `transformed (Error e) -> Error (e :> [> wig_parser_error ]))
  ;;

  type gff_parse_error =
    [ Error.parsing
    | Gff.Error.parsing
    ]

  type gff_t =
    [ track
    | Gff.item
    ]

  let string_to_gff ?filename ~tags () =
    let gff = Gff.Transform.string_to_item ?filename () in
    embed_parser ?filename (gff ~tags) ~reconstruct:(function
      | `bypassed (Ok f) -> Ok (f :> gff_t)
      | `bypassed (Error f) -> Error (f :> [> gff_parse_error ])
      | `transformed (Ok o) -> Ok (o :> gff_t)
      | `transformed (Error e) -> Error (e :> [> gff_parse_error ]))
  ;;

  type bed_parse_error =
    [ Error.parsing
    | Bed.Error.parsing
    ]

  type bed_t =
    [ track
    | Bed.item content
    ]

  let string_to_bed ?filename ?more_columns () =
    let bed = Bed.Transform.string_to_item ?more_columns () in
    embed_parser ?filename bed ~reconstruct:(function
      | `bypassed (Ok f) -> Ok (f :> bed_t)
      | `bypassed (Error f) -> Error (f :> [> bed_parse_error ])
      | `transformed (Ok o) -> Ok (`content o :> bed_t)
      | `transformed (Error e) -> Error (e :> [> bed_parse_error ]))
  ;;

  let make_printer p ~split () =
    let track = string_content_to_string ~add_content_new_line:false () in
    Tfxm.(
      compose
        (split_and_merge
           (identity ())
           p
           ~merge:
             (function
              | `left s -> s
              | `right r -> `content r)
           ~split)
        track)
  ;;

  let wig_to_string () =
    let wig = Wig.Transform.item_to_string () in
    make_printer wig () ~split:(function
      | (`comment _ | `track _ | `browser _) as x -> `left x
      | #Wig.item as y -> `right y)
  ;;

  let gff_to_string ~tags () =
    let gff = Gff.Transform.item_to_string ~tags () in
    make_printer gff () ~split:(function
      | (`comment _ | `track _ | `browser _) as x -> `left x
      | #Gff.item as y -> `right y)
  ;;

  let bed_to_string () =
    let bed = Bed.Transform.item_to_string () in
    make_printer bed () ~split:(function
      | (`comment _ | `track _ | `browser _) as x -> `left x
      | `content y -> `right y)
  ;;
end

module Test = struct
  let%expect_test "test_parser" =
    let transfo = Transform.string_to_string_content () in
    let test_line l f =
      Tfxm.feed transfo (l ^ "\n");
      printf "%s: %b\n" l (f (Tfxm.next transfo))
    in
    let test_output l o = test_line l Poly.(fun oo -> `output (Ok o) = oo) in
    test_output "# some comment" (`comment " some comment");
    test_output "browser  position   chro:42-51" (`browser (`position ("chro", 42, 51)));
    test_output "browser \t hide all" (`browser (`hide `all));
    test_output
      "browser some other command"
      (`browser (`unknown "browser some other command"));
    test_output "browser " (`browser (`unknown "browser "));
    test_output "browser" (`browser (`unknown "browser"));
    test_line "browser  position   chro:f42-51" (function
      | `output (Error (`wrong_browser_position _)) -> true
      | _ -> false);
    test_output "track a=b c=d" (`track [ "a", "b"; "c", "d" ]);
    test_output "track   \t a=b \t   c=d \t" (`track [ "a", "b"; "c", "d" ]);
    test_output "track a=\"b b\" \"c\tc c\"=d" (`track [ "a", "b b"; "c\tc c", "d" ]);
    test_output "track" (`track []);
    test_output "track   \t" (`track []);
    test_line "track a=\"b b\" \"c\tc c\"=d  \t someguyalone" (function
      | `output (Error (`wrong_key_value_format _)) -> true
      | _ -> false);
    test_output "track a=\"b b\" \"c c\"=" (`track [ "a", "b b"; "c c", "" ]);
    test_output
      "track a=\"b b\" \"c c\"=  o=c"
      (`track [ "a", "b b"; "c c", ""; "o", "c" ]);
    test_output "something else" (`content "something else");
    [%expect
      {|
      # some comment: true
      browser  position   chro:42-51: true
      browser 	 hide all: true
      browser some other command: true
      browser : true
      browser: true
      browser  position   chro:f42-51: true
      track a=b c=d: true
      track   	 a=b 	   c=d 	: true
      track a="b b" "c	c c"=d: true
      track: true
      track   	: true
      track a="b b" "c	c c"=d  	 someguyalone: true
      track a="b b" "c c"=: true
      track a="b b" "c c"=  o=c: true
      something else: true
    |}]
  ;;

  let%expect_test "test_wig_parser" =
    let transfo = Transform.string_to_wig () in
    let test_line l f =
      Tfxm.feed transfo (l ^ "\n");
      printf "%s: %b\n" l (f (Tfxm.next transfo))
    in
    let test_output l o = test_line l Poly.(fun oo -> `output (Ok o) = oo) in
    test_output "# some comment" (`comment " some comment");
    test_output "browser  position   chro:42-51" (`browser (`position ("chro", 42, 51)));
    test_output
      "variableStep chrom=chr19 span=150"
      (`variable_step_state_change ("chr19", Some 150));
    test_output "49304701 10.0" (`variable_step_value (49304701, 10.));
    test_output "49304901 12.5" (`variable_step_value (49304901, 12.5));
    [%expect
      {|
      # some comment: true
      browser  position   chro:42-51: true
      variableStep chrom=chr19 span=150: true
      49304701 10.0: true
      49304901 12.5: true
    |}]
  ;;

  let%expect_test "test_gff_parser" =
    let transfo = Transform.string_to_gff ~tags:Gff.Tags.default () in
    let test_line l f =
      Tfxm.feed transfo (l ^ "\n");
      printf "%s: %b\n" l (f (Tfxm.next transfo))
    in
    let test_output l o = test_line l Poly.(fun oo -> `output (Ok o) = oo) in
    test_output "# some comment" (`comment " some comment");
    test_output "track a=\"b b\" \"c c\"=" (`track [ "a", "b b"; "c c", "" ]);
    test_output
      (String.concat
         ~sep:"\t"
         [ "big%20spaced%20name"; "."; "."; "42"; "43"; "2e12"; "."; "."; "" ])
      (`record
        { Gff.seqname = "big spaced name"
        ; source = None
        ; feature = None
        ; pos = 42, 43
        ; score = Some 2e12
        ; strand = `not_applicable
        ; phase = None
        ; attributes = []
        });
    [%expect
      {|
      # some comment: true
      track a="b b" "c c"=: true
      big%20spaced%20name	.	.	42	43	2e12	.	.	: true
    |}]
  ;;

  let%expect_test "test_bed_parser" =
    let transfo =
      Transform.string_to_bed
        ~more_columns:(`enforce [| `type_string; `type_int; `type_float |])
        ()
    in
    let test_line l f =
      Tfxm.feed transfo (l ^ "\n");
      printf "%s: %b\n" l (f (Tfxm.next transfo))
    in
    let test_output l o = test_line l Poly.(fun oo -> `output (Ok o) = oo) in
    test_output "# some comment" (`comment " some comment");
    test_output "track a=\"b b\" \"c c\"=" (`track [ "a", "b b"; "c c", "" ]);
    test_output
      "chrA 42    45  some_string 42 3.14"
      (`content ("chrA", 42, 45, [| `string "some_string"; `int 42; `float 3.14 |]));
    test_output
      "chrB 100   130 some_string 42 3.14"
      (`content ("chrB", 100, 130, [| `string "some_string"; `int 42; `float 3.14 |]));
    [%expect
      {|
      # some comment: true
      track a="b b" "c c"=: true
      chrA 42    45  some_string 42 3.14: true
      chrB 100   130 some_string 42 3.14: true
    |}]
  ;;

  let%expect_test "test_printer" =
    let transfo = Transform.string_content_to_string () in
    let test_line i l =
      Tfxm.feed transfo i;
      printf "%s: %b\n" l Poly.(Tfxm.next transfo = `output (l ^ "\n"))
    in
    test_line (`comment "foo") "#foo";
    test_line (`browser (`hide `all)) "browser hide all";
    test_line
      (`track [ "a", "bb"; "some long", "one even longer" ])
      "track a=bb \"some long\"=\"one even longer\"";
    test_line (`content "some content") "some content";
    [%expect
      {|
      #foo: true
      browser hide all: true
      track a=bb "some long"="one even longer": true
      some content: true
    |}]
  ;;

  let%expect_test "test_wig_printer" =
    let transfo = Transform.wig_to_string () in
    let test_line i l =
      Tfxm.feed transfo i;
      printf "%s: %b\n" l Poly.(Tfxm.next transfo = `output (l ^ "\n"))
    in
    test_line (`comment "foo") "#foo";
    test_line (`browser (`hide `all)) "browser hide all";
    test_line
      (`track [ "a", "bb"; "some long", "one even longer" ])
      "track a=bb \"some long\"=\"one even longer\"";
    test_line (`fixed_step_value 42.) "42";
    [%expect
      {|
      #foo: true
      browser hide all: true
      track a=bb "some long"="one even longer": true
      42: true
    |}]
  ;;

  let%expect_test "test_gff_printer" =
    let transfo = Transform.gff_to_string ~tags:Gff.Tags.default () in
    let test_line i l =
      Tfxm.feed transfo i;
      printf "%s: %b\n" l Poly.(Tfxm.next transfo = `output (l ^ "\n"))
    in
    test_line (`comment "foo") "#foo";
    test_line (`browser (`hide `all)) "browser hide all";
    test_line
      (`track [ "a", "bb"; "some long", "one even longer" ])
      "track a=bb \"some long\"=\"one even longer\"";
    test_line
      (`record
        { Gff.seqname = "big spaced name"
        ; source = None
        ; feature = None
        ; pos = 42, 43
        ; score = Some 2.
        ; strand = `not_applicable
        ; phase = None
        ; attributes = []
        })
      "big%20spaced%20name\t.\t.\t42\t43\t2\t.\t.\t";
    [%expect
      {|
      #foo: true
      browser hide all: true
      track a=bb "some long"="one even longer": true
      big%20spaced%20name	.	.	42	43	2	.	.	: true
    |}]
  ;;

  let%expect_test "test_bed_printer" =
    let transfo = Transform.bed_to_string () in
    let test_line i l =
      Tfxm.feed transfo i;
      printf "%s: %b\n" l Poly.(Tfxm.next transfo = `output (l ^ "\n"))
    in
    test_line (`comment "foo") "#foo";
    test_line
      (`track [ "a", "bb"; "some long", "one even longer" ])
      "track a=bb \"some long\"=\"one even longer\"";
    test_line (`content ("n", 0, 1, [| `float 3.14; `int 42 |])) "n\t0\t1\t3.14\t42";
    [%expect
      {|
      #foo: true
      track a=bb "some long"="one even longer": true
      n	0	1	3.14	42: true
    |}]
  ;;
end
