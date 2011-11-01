(** MACS support. See the
    {{:http://liulab.dfci.harvard.edu/MACS/}MACS website} for details. *)

(** 
    Open (common) questions
    - there should be helpers to indicate which files are produced and then
      usable after running macs.
    - for each of these files, we'd need a parser 
*)
module type Option1 = sig
  type cmd = string * string list

  exception Error of string

  module V1_3_7_1 : sig

    val cmd : ?exec:string
      -> ?name:string -> ?format:string
      -> ?pvalue:string -> ?mfold:int32
      -> ?tsize:int32 -> ?gsize:string -> ?bw:int32
      -> ?wig:bool -> ?space:int32
      -> ?futurefdr:bool
      -> ?control:string -> treatment:string -> unit
      -> cmd

    type peak = {
      chr : string ;
      pos : Biocaml_range.t ;
      summit : int ;
      pvalue : float ;
      fdr : float option
    }

    val peaks_of_xls : string -> peak BatEnum.t
  end

  module V1_4_0 : sig

    val cmd : ?exec:string
      -> ?name:string -> ?format:string
      -> ?pvalue:string -> ?mfold:(int32 * int32)
      -> ?tsize:int32 -> ?gsize:string -> ?bw:int32
      -> ?wig:bool -> ?space:int32
      -> ?control:string -> treatment:string -> unit
      -> cmd

    type peak = V1_3_7_1.peak

    val peaks_of_xls : string -> peak BatEnum.t
  end

  (** The last version is the version by default *)
  include module type of V1_4_0
end

module Option2 = sig 

  module V1_3_7_1 : sig
  (** MACS command. *)
    type cmd = private {
      exec : string; (** path to the executable *)
      name : string option; (** other options as defined by MACS *)
      format : string option;
      pvalue : string option;
      mfold : int32 option;
      tsize : int32 option;
      gsize : string option;
      bw : int32 option;
      wig : bool;
      futurefdr : bool ;
      space : int32 option;
      control : string;
      treatment : string;
    }

    val make_cmd : ?exec:string
      -> ?name:string -> ?format:string
      -> ?pvalue:string -> ?mfold:int32
      -> ?tsize:int32 -> ?gsize:string -> ?bw:int32
      -> ?wig:bool -> ?space:int32
      -> ?futurefdr:bool
      -> control:string -> treatment:string -> unit
      -> cmd

    val cmd_to_string : cmd -> string
(** [to_string cmd] returns the string that can be typed directly on the
    command line to run MACS. *)

    type peak = {
      chr : string ;
      pos : Biocaml_range.t ;
      summit : int ;
      pvalue : float ;
      fdr : float option
    }

    val peaks_of_xls : string -> peak BatEnum.t
  end

  module V1_4_0 : sig
  (** MACS command. *)
    type cmd = private {
      exec : string; (** path to the executable *)
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

    val make_cmd : ?exec:string
      -> ?name:string -> ?format:string
      -> ?pvalue:string -> ?mfold:(int32 * int32)
      -> ?tsize:int32 -> ?gsize:string -> ?bw:int32
      -> ?wig:bool -> ?space:int32
      -> control:string -> treatment:string -> unit
      -> cmd

    val cmd_to_string : cmd -> string
(** [to_string cmd] returns the string that can be typed directly on the
    command line to run MACS. *)

    type peak = V1_3_7_1.peak

    val peaks_of_xls : string -> peak BatEnum.t
  end

  type cmd = 
      V1_3_7_1 of V1_3_7_1.cmd
    | V1_4_0 of V1_4_0.cmd

  val cmd_to_string : cmd -> string
end

