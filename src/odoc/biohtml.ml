

open Printf (* don't know how to link Core with ocamldoc  *)
open Odoc_info
(* open Module *)
(* module StringSet = Odoc_html.StringSet *)

let word_re = Str.regexp "[ \t\r\n]+"

let split_args t =
  match t with
  | [] -> []
  | [Odoc_info.Raw arg] -> Str.split word_re arg
  | _ ->
    failwith "Argument too complicated (you should not use markup in captions)"

(*
let read_all f =
  let i = open_in f in
  let b = Buffer.create 1024 in
  let rec loop () =
    try
      Buffer.add_char b (input_char i);
      loop ()
    with e -> ()
  in
  loop ();
  close_in i;
  Buffer.contents b
*)

module Generator (G : Odoc_html.Html_generator) =
struct
  class html =
  object(self)
    inherit G.html as super

    (* method generate module_list = *)
      (* eprintf "generate Called !\n%!" *)

    method private html_of_figure b t =
      let (file, width, caption) =
        let read_width s =
          try `percent (Scanf.sscanf s "%d%%" (fun i -> i))
          with e ->
            ksprintf failwith "in {figure _} expecting width but got %S" s in
        match split_args t with
	| [] -> failwith "{figure ...} needs at least one argument"
        | one :: [] -> (one, `percent 90, "")
        | one :: two :: [] -> (one, read_width two, "")
        | one :: two :: three -> (one, read_width two, String.concat " " three)
      in
      bprintf b "
        <div class=\"figure\" style=\"max-width : %s\" title=%S>
        <img src=\"%s\" width=\"99%%\"/>
        <div class=\"caption\">%s</div>
        </div>"
        (match width with `percent s -> sprintf "%d%%" s) file
        (Filename.basename file)
        caption

    method html_of_custom_text b s t =
      eprintf "html_of_custom_text Called s: %S \n%!" s;
      match s with
      | "figure" -> self#html_of_figure b t
      | _ -> ()
  end
end

let _ = Odoc_args.extend_html_generator (module Generator : Odoc_gen.Html_functor)

  (*
let biohtml = new biohtml
let _ =
  Odoc_args.set_doc_generator (Some biohtml :> Odoc_args.doc_generator option)
  *)
