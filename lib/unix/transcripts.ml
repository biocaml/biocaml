open Core_kernel
(*
type 'a transcript = {
  exons : (int * int) list;
  lo : int;
  hi : int;
  chr : string;
  info : 'a
}

type 'a t = 'a transcript list

module II = struct
  type t = int * int
  let compare = Pervasives.compare
end

module SIIMap = MMap.Make(String)(II)
module SSMap = MMap.Make(String)(String)

let add_length_to_transcripts transcripts =
  let f trx =
    let length =
      let g acc (lo,hi) = hi - lo + acc in
      List.fold_left g 0 trx.exons
    in
    { trx with info = trx.info,length }
  in
  List.map f transcripts

let of_composite_channel
    ?(chr_map=identity)
    ?(increment_lo_hi=(0,0))
    ic =
  let f acc l =
    let lst = String.nsplit l "\t" in
    let inclo,inchi = increment_lo_hi in
    let (nm,chr,st,fn) =
      List.nth lst 0,
      chr_map (List.nth lst 1),
      int_of_string (List.nth lst 2) + inclo,
      int_of_string (List.nth lst 3) + inchi
    in
    let g (nm,chr,st,fn) prev = match prev with
      | None ->
          {
            exons = [st,fn];
            lo = st;
            hi= fn;
            chr = chr;
            info = nm;
          }
      | Some trx ->
          {
            exons = (st,fn)::(trx.exons);
            lo = if st < trx.lo then st else trx.lo;
            hi = if fn > trx.hi then fn else trx.hi;
            chr = chr;
            info = nm;
          }
    in
    SSMap.add_with nm chr (g (nm,chr,st,fn)) acc
  in
  let ans = In_channel.fold_lines ic ~init:SSMap.empty ~f in
  let folder k1 k2 acc elem = elem::acc in
  let ans = List.rev (SSMap.fold folder [] ans) in
  add_length_to_transcripts ans

let of_composite_file ?(chr_map=identity) ?(increment_lo_hi=(0,0)) file =
  try_finally
    (of_composite_channel ~chr_map ~increment_lo_hi) close_in (open_in file)

let of_bed_channel ?(chr_map=identity) ?(increment_lo_hi=(1,0)) ic =
  let bed = Bed.to_list (Bed.of_channel ~chr_map ~increment_lo_hi ic) in
  let f acc (chr,s,f) =
    {
      exons = [s,f];
      lo = s;
      hi = f;
      chr = chr_map chr;
      info = "";
    }::acc
  in
  let ans = List.rev (List.fold_left f [] bed) in
  add_length_to_transcripts ans

let of_bed_file ?(chr_map=identity) ?(increment_lo_hi=(1,0)) file =
  try_finally (of_bed_channel ~chr_map ~increment_lo_hi) close_in (open_in file)

let of_gff transcript_name_of_exon gff =
  let f transcript_name row prev =
    let lo,hi = row.Gff.pos in
    match prev with
      | None -> {
          exons = [row.Gff.pos];
          lo = lo;
          hi = hi;
          chr = row.Gff.chr;
          info = transcript_name
        }
      | Some prev -> {
          exons = row.Gff.pos::prev.exons;
          lo = if lo < prev.lo then lo else prev.lo;
          hi = if hi > prev.hi then hi else prev.hi;
          chr = if row.Gff.chr = prev.chr then row.Gff.chr
            else failwithf "chromosome of transcript %s changed from %s to %s"
              transcript_name prev.chr row.Gff.chr ();
          info = (assert (transcript_name = prev.info); transcript_name)
        }
  in
  let g ans row = match transcript_name_of_exon row with
    | None -> ans
    | Some transcript_name ->
      StringMap.add_with transcript_name (f transcript_name row) ans
  in
  let ans = Gff.fold g StringMap.empty gff in
  StringMap.fold (fun _ x ans -> x::ans) ans []

let all_probes_in
    (trx_lst:'a t)
    (prbs: (string * int * int * 'b) list)
    : ('a * 'b array) t =
  let insert x prev = match prev with None -> [x] | Some l -> x::l in
  let siimap_of_exons =
    let f acc trx =
      SIIMap.add trx.chr (trx.lo,trx.hi) (trx.exons,trx.info) acc in
    List.fold_left f SIIMap.empty trx_lst
  in
  let stringmap_of_intervaltrees =
    let f acc trx = StringMap.add_with trx.chr (insert (trx.lo,trx.hi)) acc in
    let ans = List.fold_left f StringMap.empty trx_lst in
    StringMap.map IntervalTree.create ans
  in
  let f acc (chr,s,f,v) =
    let itree = StringMap.find chr stringmap_of_intervaltrees in
    let trxs = IntervalTree.within itree (s,f) in
    let g accu trx =
      let (exons,info) = SIIMap.find chr trx siimap_of_exons in
      let g_insert (info,u) prev =
        match prev with
          | None -> info,[v]
          | Some (i,lst) -> (assert (i = info); i,(v::lst))
      in
      match IntervalTree.within (IntervalTree.create exons) (s,f) with
        | [] -> accu
        | a::b -> SIIMap.add_with chr trx (g_insert (info,v)) accu
    in
    List.fold_left g acc trxs
  in
  let ans = List.fold_left f SIIMap.empty prbs in
  let ans = SIIMap.map (fun (info,lst) -> (info,Array.of_list lst)) ans in
  let f acc trx =
    try
      {
        exons = trx.exons;
        lo = trx.lo;
        hi = trx.hi;
        chr = trx.chr;
        info = SIIMap.find trx.chr (trx.lo,trx.hi) ans
      }::acc
    with Not_found -> acc
  in
  List.rev (List.fold_left f [] trx_lst)

let all_points_in
    (trx_lst:'a t)
    (points: (string * int * 'b) list)
    : ('a * 'b array) t =
  let probes = List.map (fun (x,y,z) -> (x,y,y,z)) points in
  all_probes_in trx_lst probes
*)
