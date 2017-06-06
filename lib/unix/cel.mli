(** Affymetrix's CEL files. Only text format supported. Binary file
    must be converted using Affymetrix's conversion tool. This tool does
    not change file extension, so be sure your file really is in text
    format. *)
open Core_kernel

type t
(** The type of a CEL file. *)

exception Bad of string
(** Raised when encountering ill-formed CEL type. *)

val of_file : string -> t
(** Parse given file if possible. Raise [Bad] if there are any parse errors. *)

val of_file_opt : string -> t option
(** Parse given file if possible. *)


(** {6 Operations on Intensity Section} *)

type idata = {
  mean:float; (** mean intensity value *)
  stdv:float; (** standard deviation of intensity *)
  npixels:int (** number of pixels used in mean/stdv calculation *)
}

(** Represents row in intensity section *)
type irow = {
  xcoord:int; (** x-coordinate *)
  ycoord:int; (** y-coordinate *)
  idata:idata
}

val ifold : ('a -> irow -> 'a) -> 'a -> t -> 'a
  (** [ifold f a t] folds over data rows in intensity section of [t]. *)

val iiter : (irow -> unit) -> t -> unit
  (** [iiter f t] iterates over the rows in intensity section of [t]. *)

val data :
  Bpmap.t -> t list -> (Bpmap.probe * (idata * idata) list) list
(** [data bpmap cels] returns a list associating probes with pairs of
    (PM,MM) idata in each of the given cel files (in the same order of
    course). Raise [Failure] if any file in [cels] lacks a value for any
    probe in [bpmap]. *)

val pm_mm : Bpmap.t -> t list -> (Bpmap.probe * float list) list
(** Similar to {!data} but the data returned are the PM-MM mean
    intensity values in [cels]. *)

val pm : Bpmap.t -> t list -> (Bpmap.probe * float list) list
(** Similar to {!data} but the data returned are the PM mean intensity
    values in [cels]. *)

val mm : Bpmap.t -> t list -> (Bpmap.probe * float list) list
(** Similar to {!data} but the data returned are the MM mean intensity
    values in [cels]. *)
