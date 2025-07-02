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

(** The type of the GFF records/rows. *)
type record =
  { seqname : string
  ; source : string option
  ; feature : string option
  ; pos : int * int
  ; score : float option
  ; strand : [ `plus | `minus | `not_applicable | `unknown ]
  ; phase : int option
  ; attributes : (string * string list) list
  }

(** The items being output by the parser. *)
type item =
  [ `comment of string
  | `record of record
  ]

(** {2 Error Types} *)

module Error : sig
  (** The errors of the [Gff] module. *)

  (** The possible parsing errors. *)
  type parsing =
    [ `cannot_parse_float of Biocaml.Pos.t * string
    | `cannot_parse_int of Biocaml.Pos.t * string
    | `cannot_parse_strand of Biocaml.Pos.t * string
    | `cannot_parse_string of Biocaml.Pos.t * string
    | `empty_line of Biocaml.Pos.t
    | `incomplete_input of Biocaml.Pos.t * string list * string option
    | `wrong_attributes of Biocaml.Pos.t * string
    | `wrong_row of Biocaml.Pos.t * string
    | `wrong_url_escaping of Biocaml.Pos.t * string
    ]

  (** The union of all the errors of this module. *)
  type t = parsing

  val parsing_of_sexp : Sexplib.Sexp.t -> parsing
  val sexp_of_parsing : parsing -> Sexplib.Sexp.t
  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
end

(** {2 {!Tags.t} } *)

module Tags : sig
  (** Additional format-information tags (c.f. {!Tags}). *)
  type t =
    { version : [ `two | `three ]
    ; allow_empty_lines : bool
    ; sharp_comments : bool
    }

  (** Default tags for a random Gff file:
      [{version = `three; allow_empty_lines = false; sharp_comments = true}]. *)
  val default : t

  (** Parse tags (for now S-Expressions). *)
  val of_string : string -> (t, [> `gff of [> `tags_of_string of exn ] ]) result

  (** Serialize tags (for now S-Expressions). *)
  val to_string : t -> string

  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
end

(** {2 [In_channel.t] Functions } *)

(** The exception raised by the [*_exn] functions. *)
exception Error of Error.t

(** Parse an input-channel into [item] values. *)
val in_channel_to_item_stream
  :  ?buffer_size:int
  -> ?filename:string
  -> ?tags:Tags.t
  -> In_channel.t
  -> (item, [> Error.parsing ]) result Stream.t

(** Like [in_channel_to_item_stream] but use exceptions for errors
    (raised within [Stream.next]). *)
val in_channel_to_item_stream_exn
  :  ?buffer_size:int
  -> ?tags:Tags.t
  -> In_channel.t
  -> item Stream.t

(** {2 [To_string] Function } *)

(** Convert an item to a string. *)
val item_to_string : ?tags:Tags.t -> item -> string

(** {2 {!Tfxm.t} } *)

module Transform : sig
  (** Lower-level stream transformations. *)

  (** Create a parsing [Biocaml_transform.t] for a given version. *)
  val string_to_item
    :  ?filename:string
    -> tags:Tags.t
    -> unit
    -> (string, (item, [> Error.parsing ]) result) Tfxm.t

  (** Create a printer for a given version. *)
  val item_to_string : tags:Tags.t -> unit -> (item, string) Tfxm.t
end

(** {2 S-Expressions } *)

val record_of_sexp : Sexplib.Sexp.t -> record
val sexp_of_record : record -> Sexplib.Sexp.t
val item_of_sexp : Sexplib.Sexp.t -> item
val sexp_of_item : item -> Sexplib.Sexp.t
