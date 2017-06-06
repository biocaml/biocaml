(** GFF files.

    Versions 2 and 3 are supported. The only difference is the
    delimiter used for tag-value pairs in the attribute list: [3] uses
    an equal sign, and [2] uses a space. Version [3] also has
    additional requirements, e.g. the [feature] must be a sequence
    ontology term, but these are not checked.


    More information: {ul
      {- Version 2:
         {{:http://www.sanger.ac.uk/resources/software/gff/spec.html
            }www.sanger.ac.uk/resources/software/gff/spec.html},
         {{:http://gmod.org/wiki/GFF2}gmod.org/wiki/GFF2}
      }
      {- Version 3:
         {{:http://www.sequenceontology.org/gff3.shtml
             }www.sequenceontology.org/gff3.shtml},
         {{:http://gmod.org/wiki/GFF3
            }gmod.org/wiki/GFF3}
      }
    }
*)

(** {2 GFF Item Types} *)

type record = {
  seqname: string;
  source: string option;
  feature: string option;
  pos: int * int;
  score: float option;
  strand: [`plus | `minus | `not_applicable | `unknown ];
  phase: int option;
  attributes: (string * string list) list;
}
(** The type of the GFF records/rows. *)

type item = [ `comment of string | `record of record ]
(** The items being output by the parser. *)

(** {2 Error Types} *)

module Error: sig
  (** The errors of the [Gff] module. *)


  type parsing =
    [ `cannot_parse_float of Pos.t * string
    | `cannot_parse_int of Pos.t * string
    | `cannot_parse_strand of Pos.t * string
    | `cannot_parse_string of Pos.t * string
    | `empty_line of Pos.t
    | `incomplete_input of Pos.t * string list * string option
    | `wrong_attributes of Pos.t * string
    | `wrong_row of Pos.t * string
    | `wrong_url_escaping of Pos.t * string ]
  (** The possible parsing errors. *)

  type t = parsing
  (** The union of all the errors of this module. *)

  val parsing_of_sexp : Sexplib.Sexp.t -> parsing
  val sexp_of_parsing : parsing -> Sexplib.Sexp.t
  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
end

(** {2 {!Tags.t} } *)

module Tags: sig

  type t = {
    version: [`two | `three];
    allow_empty_lines: bool;
    sharp_comments: bool;
  }
  (** Additional format-information tags (c.f. {!Tags}). *)

  val default: t
  (** Default tags for a random Gff file:
      [{version = `three; allow_empty_lines = false; sharp_comments = true}]. *)

  val of_string: string ->
    (t, [> `gff of [> `tags_of_string of exn ] ]) result
  (** Parse tags (for now S-Expressions). *)

  val to_string: t -> string
  (** Serialize tags (for now S-Expressions). *)

  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
end

(** {2 [In_channel.t] Functions } *)

exception Error of  Error.t
(** The exception raised by the [*_exn] functions. *)

val in_channel_to_item_stream : ?buffer_size:int -> ?filename:string ->
  ?tags:Tags.t -> in_channel ->
  (item, [> Error.parsing]) result Stream.t
(** Parse an input-channel into [item] values. *)

val in_channel_to_item_stream_exn : ?buffer_size:int -> ?tags:Tags.t ->
  in_channel -> item Stream.t
(** Like [in_channel_to_item_stream] but use exceptions for errors
    (raised within [Stream.next]). *)

(** {2 [To_string] Function } *)

val item_to_string: ?tags:Tags.t -> item -> string
(** Convert an item to a string. *)

(** {2 {!Tfxm.t} } *)

module Transform: sig
  (** Lower-level stream transformations. *)

  val string_to_item:
    ?filename:string ->
    tags: Tags.t ->
    unit ->
    (string, (item, [> Error.parsing]) result) Tfxm.t
  (** Create a parsing [Biocaml_transform.t] for a given version. *)

  val item_to_string: tags: Tags.t -> unit ->
    (item, string) Tfxm.t
  (** Create a printer for a given version. *)

end


(** {2 S-Expressions } *)

val record_of_sexp : Sexplib.Sexp.t -> record
val sexp_of_record : record -> Sexplib.Sexp.t
val item_of_sexp : Sexplib.Sexp.t -> item
val sexp_of_item : item -> Sexplib.Sexp.t
