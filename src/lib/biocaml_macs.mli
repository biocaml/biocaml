(** MACS support. See the
    {{:http://liulab.dfci.harvard.edu/MACS/}MACS website} for details. *)

(** 
    Open (common) questions
    - there should be helpers to indicate which files are produced and then
      usable after running macs.
    - for each of these files, we'd need a parser 
*)

(* should be defined elsewhere *)
type path = string
type shell_cmd = path * string list

exception Error of string

module Ver_1_4_0 : sig

  type cmd = private {
    exec : path; (** path to the executable *)
    name : string option; (** other options as defined by MACS *)
    format : string option;
    pvalue : string option;
    mfold : (int32 * int32) option;
    tsize : int32 option;
    gsize : string option;
    bw : int32 option;
    wig : bool;
    space : int32 option;
    control : string;
    treatment : string;
  }

  (** checks if the version of macs at path [path] is
      supported by this module *)
  val check_version : path -> bool

  val cmd : ?exec:path
    -> ?name:string -> ?format:string
    -> ?pvalue:string -> ?mfold:(int32 * int32)
    -> ?tsize:int32 -> ?gsize:string -> ?bw:int32
    -> ?wig:bool -> ?space:int32
    -> ?control:string -> treatment:string -> unit
    -> cmd

  val xls_output : cmd -> path
  val bed_output : cmd -> path
  val rprogram_output : cmd -> path

  type peak = {
    chr : string ;
    pos : Biocaml_range.t ;
    summit : int ;
    pvalue : float ;
    fdr : float option
  }

  val peaks : cmd -> peak Enum.t

end
