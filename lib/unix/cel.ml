open Core_kernel
open CFStream

type idata = {mean:float; stdv:float; npixels:int}
type irow = {xcoord:int; ycoord:int; idata:idata}
type isection = irow list
let icolumns = ["X";"Y";"MEAN";"STDV";"NPIXELS"]
let isection_name = "INTENSITY"
let inum_values = List.length
let ifold f init l = List.fold_left ~f ~init l
let iiter f l = List.iter ~f l

type t = isection (* retaining only intensity section for now *)
exception Bad of string
let raise_bad msg = raise (Bad msg)

(* Hashtbl representation of intensity data. *)
module Tbl = struct

  module HT = Caml.Hashtbl

  (* hash table of (mean,stdv,npixels) data *)
  let of_cel1 (cel:t) : (int * int, idata) HT.t =
    let tbl = HT.create (inum_values cel) in
    let f r = HT.add tbl (r.xcoord,r.ycoord) r.idata in
      iiter f cel; tbl

  (* hash table of mean data *)
  let of_cel2 (cel:t) : (int * int, float) HT.t =
    let tbl = HT.create (inum_values cel) in
    let f r = HT.add tbl (r.xcoord,r.ycoord) r.idata.mean in
      iiter f cel; tbl

  (* like HT.find but throws more informative exception *)
  let find tbl (x,y) =
    try HT.find tbl (x,y)
    with Not_found ->
      failwith (Msg.err (sprintf "CEL file does not have values for probe \
                                  at position x = %d, y = %d" x y))
end

let data bpmap cels =
  let cels = List.map ~f:Tbl.of_cel1 cels in
  let f ans r =
    let datl =
      List.map  cels ~f:(fun cel ->
        Tbl.find cel r.Bpmap.pmcoord, Tbl.find cel r.Bpmap.mmcoord) in
    (r.Bpmap.probe, datl) :: ans
  in
  Bpmap.fold f [] bpmap

let pm_mm bpmap cels =
  let cels = List.map ~f:Tbl.of_cel2 cels in
  let f ans r =
    let datl =
      List.map cels ~f:(fun cel ->
        (Tbl.find cel r.Bpmap.pmcoord) -. (Tbl.find cel r.Bpmap.mmcoord)) in
    (r.Bpmap.probe, datl) :: ans
  in
  Bpmap.fold f [] bpmap

let pm bpmap cels =
  let cels = List.map ~f:Tbl.of_cel2 cels in
  let f ans r =
    let datl = List.map cels ~f:(fun cel -> Tbl.find cel r.Bpmap.pmcoord) in
    (r.Bpmap.probe, datl) :: ans
  in
  Bpmap.fold f [] bpmap

let mm bpmap cels =
  let cels = List.map ~f:Tbl.of_cel2 cels in
  let f ans r =
    let datl = List.map ~f:(fun cel -> Tbl.find cel r.Bpmap.mmcoord) cels in
    (r.Bpmap.probe, datl) :: ans
  in
  Bpmap.fold f [] bpmap

module Parser = struct
  (** parse string of form "\[sss\]" *)
  let section_name s =
    let s = String.strip s in
    let l = String.length s in
      if l < 2 || not (s.[0] = '[' && s.[l-1] = ']')
      then None
      else Some (String.slice s 1 (l-1))

  let line_is_section sec_name l =
    match section_name l with
      | None -> false
      | Some s -> s = sec_name

  let intensity_row s =
    let to_int s = Int.of_string (String.strip s) in
    let to_float s = Float.of_string (String.strip s) in
      match String.split s ~on:'\t' with
      | [xcoord; ycoord; mean; stdv; npixels] ->
            {
              xcoord = to_int xcoord;
              ycoord = to_int ycoord;
              idata =
                {
                  mean = to_float mean;
                  stdv = to_float stdv;
                  npixels = to_int npixels
                }
            }
      | _ -> raise_bad "expecting 5 columns"

  (** lines should point to beginning of intensity section,
      upon return lines will point to first blank line after data rows  *)
  let intensity_section lines =
    assert (
      match Stream.peek lines with
      | None -> false
        | Some l -> line_is_section isection_name l
    );
    Stream.junk lines;

    let sl = String.split (Stream.next_exn lines) ~on:'=' in
    let num_cells = int_of_string (String.strip (List.nth_exn sl 1)) in

    let sl = String.split (Stream.next_exn lines) ~on:'=' in
    let sl = String.split (List.nth_exn sl 1) ~on:'\t' in
    let sl = List.map ~f:String.strip sl in
    let _ =
      if sl <> icolumns then
        raise_bad "intensity section column names incorrect" in

    let lines =
      Stream.take_while
        ~f:(fun s -> not (String.for_all s ~f:Char.is_whitespace)) lines in
    let lines = Stream.map ~f:intensity_row lines in
    let ans = Stream.to_list lines in
    let count = List.length ans in
      if count = num_cells then ans
      else raise_bad (sprintf "expected %d intensity rows but found %d" num_cells count)

  let cel file =
    let of_channel cin =
      let lines = Lines.of_channel cin in
      let err msg = Msg.err ~pos:(Pos.make ~source:file ~line:(Stream.count lines) ()) msg in
      try
        Stream.drop_while ~f:(fun (s : Lines.item) -> not (line_is_section isection_name (s :> string))) lines;
        if Stream.is_empty lines
        then failwith (isection_name ^ " section not found");
        intensity_section (Stream.map lines ~f:(fun (x : Lines.item) -> (x :> string)))
      with
        Failure msg | Bad msg -> raise_bad (err msg)
    in
    In_channel.with_file file ~f:of_channel

end

let of_file = Parser.cel
let of_file_opt file = try Some (of_file file) with Bad _ -> None
