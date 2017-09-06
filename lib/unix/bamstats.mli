open Core_kernel

type t = {
  total : int ;
  qc_pass : int ; (** [not_passing_quality_controls] returns [false],
                      assumed for all other counts *)
  single_reads : int ; (** [has_multiple_segments] returns [false] *)
  read_pairs : int ; (** [has_multiple_segments] and [first_segment] *)
  mapped_reads : int ; (** [!segment_unmapped] and [!secondary_alignment]
                           and [!supplementary_alignment] *)
  mapped_pairs : int ; (** [has_multiple_segments] and [first_segment]
                           and [each_segment_properly_aligned]
                           and [!secondary_alignment]
                           and [!supplementary_alignment] *)
}
[@@deriving sexp]

val zero : t
val update0 : t -> Bam.Alignment0.t -> t Or_error.t
val update : t -> Sam.alignment -> t
