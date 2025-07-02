(** {2 Input/Output } *)
module MakeIO (Future : Future.S) : sig
  open Future

  val read : Reader.t -> Biocaml.Fastq.item Or_error.t Pipe.Reader.t
  val write : Writer.t -> Biocaml.Fastq.item Pipe.Reader.t -> unit Deferred.t

  val write_file
    :  ?perm:int
    -> ?append:bool
    -> string
    -> Biocaml.Fastq.item Pipe.Reader.t
    -> unit Deferred.t
end

include module type of MakeIO (Future_unix)
