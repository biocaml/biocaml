(** SAM files. Documentation here assumes familiarity with the {{:
    http://samtools.github.io/hts-specs/SAMv1.pdf } SAM
    specification}. *)

(******************************************************************************)

(******************************************************************************)

(** {2 Input/Output } *)
module MakeIO (Future : Future.S) : sig
  open Future

  val read
    :  ?start:Biocaml.Pos.t
    -> Reader.t
    -> (Biocaml.Sam.header * Biocaml.Sam.alignment Or_error.t Pipe.Reader.t) Or_error.t
       Deferred.t

  val write
    :  Writer.t
    -> ?header:Biocaml.Sam.header
    -> Biocaml.Sam.alignment Pipe.Reader.t
    -> unit Deferred.t

  val write_file
    :  ?perm:int
    -> ?append:bool
    -> string
    -> ?header:Biocaml.Sam.header
    -> Biocaml.Sam.alignment Pipe.Reader.t
    -> unit Deferred.t
end

include module type of MakeIO (Future_unix)

val parse_header : string -> Biocaml.Sam.header Or_error.t
