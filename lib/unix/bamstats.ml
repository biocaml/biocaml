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

let zero = {
  total = 0 ;
  qc_pass = 0 ;
  single_reads = 0 ;
  read_pairs = 0 ;
  mapped_reads = 0 ;
  mapped_pairs = 0 ;
}


let incr_if b i = if b then i + 1 else i

let update_gen s flags =
  let open Sam.Flags in
  let total = s.total + 1 in
  if not_passing_quality_controls flags then
    { s with total }
  else
    let qc_pass = s.qc_pass + 1 in
    let single_reads = incr_if (not (has_multiple_segments flags)) s.single_reads in
    let pair_witness = has_multiple_segments flags && first_segment flags in
    let read_pairs = incr_if pair_witness s.read_pairs in
    let main_mapping =
      not (segment_unmapped flags ||
           secondary_alignment flags ||
           supplementary_alignment flags)
    in
    let mapped_reads = incr_if main_mapping s.mapped_reads in
    let mapped_pairs = incr_if (main_mapping && pair_witness) s.mapped_pairs in
    { total ; qc_pass ; single_reads ; read_pairs ; mapped_reads ; mapped_pairs }

let update s al =
  update_gen s al.Sam.flags

let update0 s al =
  let open Or_error.Monad_infix in
  Bam.Alignment0.flags al
  >>| update_gen s



module Fragment_length_histogram = struct
  type t = {
    min_mapq : int ;
    counts : int Accu.Counter.t ;
  }

  let create ?(min_mapq = 0) () = {
    min_mapq ;
    counts = Accu.Counter.create () ;
  }

  let with_mapped_pair ~min_mapq al f =
    let open Or_error.Monad_infix in
    Bam.Alignment0.flags al >>= fun fl ->
    let multi_segment = Sam.Flags.has_multiple_segments fl in
    let each_segment_properly_aligned = Sam.Flags.each_segment_properly_aligned fl in
    let segment_unmapped = Sam.Flags.segment_unmapped fl in
    let qc_ok = match Bam.Alignment0.mapq al with
      | Some mapq -> mapq >= min_mapq
      | None -> false
    in
    let mapped_fragment =
      multi_segment && not segment_unmapped && each_segment_properly_aligned
    in
    if mapped_fragment && qc_ok then f () else Ok ()

  let update0 { min_mapq ; counts } al =
    with_mapped_pair ~min_mapq al (fun () ->
        let length = match Bam.Alignment0.tlen al with
          | Some l -> Int.abs l
          | _ -> assert false (* both reads are mapped so there should be a length *)
        in
        Accu.Counter.tick counts length ;
        Ok ()
      )

end

module Chr_histogram = struct
  type t = {
    min_mapq : int ;
    bam_header : Bam.Header.t ;
    counts : string Accu.Counter.t ;
  }

  let create ?(min_mapq = 0) bam_header = {
    min_mapq ;
    bam_header ;
    counts = Accu.Counter.create () ;
  }

  let update0 { min_mapq ; counts ; bam_header } al =
    Fragment_length_histogram.with_mapped_pair ~min_mapq al (fun () ->
        match Bam.Alignment0.rname al bam_header with
        | Ok (Some chr) ->
          Accu.Counter.tick counts chr ;
          Ok ()
        | Ok None -> Ok ()
        | Error _ as e -> e
      )
end
