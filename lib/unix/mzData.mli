(** Read mzData files (mass spectrometry data format).

 *)
open Core_kernel
open Bigarray

type vec = (float, float64_elt, fortran_layout) Array1.t
(** Vectors of 64 bits floats. *)

type int_vec = (int, int_elt, fortran_layout) Array1.t
(** Vectors of OCaml ints. *)


module Precursor : sig
  type t = {
    mslevel: int; (** 1: MS, 2: MS/MS,... *)
    mz: float;    (** MassToChargeRatio *)
    z: float;     (** ChargeState *)
    int: float;   (** Intensity *)
  }

  val mass : t -> float
  (** [mass p] return the mass of the precursor [p] {i without} charge. *)
end


(** Individual mass spectrum. *)
type spectrum = {
  id: int;         (** index of the spectrum in the file (starting with 1). *)
  mslevel: int;    (** 1: MS, 2: MS/MS,... *)
  precursor: Precursor.t list;  (** List of precursors to the spectrum
                                    currently being described. *)
  mz: vec;         (** m/z *)
  int: vec;        (** intensities *)
  sup: (string * vec) list; (** Supplemental (name, arrays), if any *)
}

val of_file : string -> spectrum list
(** [of_file fname] returns the spectra contained in the file
    [fname].  *)

;;
