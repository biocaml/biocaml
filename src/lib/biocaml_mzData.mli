(** Read mzData files (mass spectrometry data format).

 *)

open Bigarray

type vec = (float, float64_elt, fortran_layout) Array1.t
(** Vectors of 64 bits floats. *)

type int_vec = (int, int_elt, fortran_layout) Array1.t
(** Vectors of OCaml ints. *)


(** MS spectrum. *)
type spectrum = {
  id: int;         (** index of the spectrum in the file (starting with 1). *)
  mslevel: int;    (** 1: MS, 2: MS/MS,... *)
  mass: float;     (** total peptide mass *)
  start_mz: float;
  end_mz: float;
  mz: vec;         (** m/z *)
  int: vec;        (** intensities *)
  z: int_vec;      (** Charge State *)
}

val spectrums : string -> spectrum list
(** [spectrums fname] returns the spectrums contained in the file
    [fname]. *)

;;
