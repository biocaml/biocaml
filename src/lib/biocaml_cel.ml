open Biocaml_std

module Bpmap = Biocaml_bpmap

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
  (* hash table of (mean,stdv,npixels) data *)
  let of_cel1 (cel:t) : (int * int, idata) Hashtbl.t =
    let tbl = Hashtbl.create (inum_values cel) in
    let f r = Hashtbl.add tbl (r.xcoord,r.ycoord) r.idata in
      iiter f cel; tbl
        
  (* hash table of mean data *)
  let of_cel2 (cel:t) : (int * int, float) Hashtbl.t =
    let tbl = Hashtbl.create (inum_values cel) in
    let f r = Hashtbl.add tbl (r.xcoord,r.ycoord) r.idata.mean in
      iiter f cel; tbl
        
  (* like Hashtbl.find but throws more informative exception *)
  let find tbl (x,y) =
    try Hashtbl.find tbl (x,y)
    with Not_found -> failwith (Msg.err (sprintf "CEL file does not have values for probe at position x = %d, y = %d" x y))
end
  
let data bpmap cels =
  let cels = List.map Tbl.of_cel1 cels in
  let f ans r =
    let datl = List.map (fun cel -> Tbl.find cel r.Bpmap.pmcoord, Tbl.find cel r.Bpmap.mmcoord) cels in
    (r.Bpmap.probe,datl)::ans
  in
  Bpmap.fold f [] bpmap
    
let pm_mm bpmap cels =
  let cels = List.map Tbl.of_cel2 cels in
  let f ans r =
    let datl = List.map (fun cel -> (Tbl.find cel r.Bpmap.pmcoord) -. (Tbl.find cel r.Bpmap.mmcoord)) cels in
    (r.Bpmap.probe,datl)::ans
  in
  Bpmap.fold f [] bpmap
    
let pm bpmap cels =
  let cels = List.map Tbl.of_cel2 cels in
  let f ans r =
    let datl = List.map (fun cel -> Tbl.find cel r.Bpmap.pmcoord) cels in
    (r.Bpmap.probe,datl)::ans
  in
  Bpmap.fold f [] bpmap

let mm bpmap cels =
  let cels = List.map Tbl.of_cel2 cels in
  let f ans r =
    let datl = List.map (fun cel -> Tbl.find cel r.Bpmap.mmcoord) cels in
    (r.Bpmap.probe,datl)::ans
  in
  Bpmap.fold f [] bpmap
    
module Parser = struct
  (** parse string of form "\[sss\]" *)
  let section_name s =
    let s = String.strip s in
    let l = String.length s in
      if l < 2 || not (s.[0] = '[' && s.[l-1] = ']')
      then None
      else Some (String.slice ~first:1 ~last:(l-1) s)
        
  let section_name_exn s =
    match section_name s with 
      | Some s -> s
      | None -> raise_bad ("invalid section name " ^ s)
          
  let line_is_section sec_name l =
    match section_name l with
      | None -> false
      | Some s -> s = sec_name

  let intensity_row s =
    let sl = String.nsplit s "\t" in
    let to_int = int_of_string <<- String.strip <<- (List.nth sl) in
    let to_float = float_of_string <<- String.strip <<- (List.nth sl) in
      match List.length sl with
        | 5 ->
            {
              xcoord = to_int 0;
              ycoord = to_int 1;
              idata =
                {
                  mean = to_float 2;
                  stdv = to_float 3;
                  npixels = to_int 4
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
    
    let sl = String.nsplit (Stream.next lines) "=" in
    let num_cells = (int_of_string <<- String.strip <<- (List.nth sl)) 1 in

    let sl = String.nsplit (Stream.next lines) "=" in
    let sl = String.nsplit (List.nth sl 1) "\t" in
    let sl = List.map String.strip sl in
    let _ = if sl <> icolumns then raise_bad "intensity section column names incorrect" else () in

    let lines = Stream.keep_while (not <<- (String.for_all ~f:Char.is_space)) lines in
    let lines = Stream.map intensity_row lines in
    let ans = Stream.to_list lines in
    let count = List.length ans in
      if count = num_cells then ans
      else raise_bad (sprintf "expected %d intensity rows but found %d" num_cells count)

  let cel file =
    let of_channel cin =
      let lines = Stream.lines_of_channel cin in
      let err msg = Msg.err ~pos:(Pos.fl file (Stream.count lines)) msg in
        try
          Stream.skip_while (not <<- line_is_section isection_name) lines;
          if Stream.is_empty lines then failwith (isection_name ^ " section not found");
          intensity_section lines
        with
            Failure msg | Bad msg -> raise_bad (err msg)
    in
      try_finally of_channel close_in (open_in file)
end

let of_file = Parser.cel
let of_file_opt file = try Some (of_file file) with Bad _ -> None
