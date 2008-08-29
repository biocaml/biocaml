open TylesBase
open Printf

type probe = {org_name:string; version:string; chr_name:string; start_pos:int; sequence:Seq.t}
type row = {pmcoord:int*int; mmcoord:int*int; probe:probe}

type t = row list
exception Bad of string
let raise_bad msg = raise (Bad msg)

let col_names = ["PMX";"PMY";"MMX";"MMY";"Seq";"Pos";"Probe"]
let num_probes = List.length  
let iter = List.iter
let fold = List.fold_left
let row_list t = t

module Parser = struct
  let header (s:string) : string list =
    let sl = String.nsplit s "\t" in
      if sl = col_names then sl
      else raise_bad "incorrectly formatted header"
        
  let row (s:string) : row =
    let sl = String.nsplit s "\t" in
    let to_int = int_of_string <<- (List.nth sl) in
    let to_string = List.nth sl in
    let _ = (if List.length sl <> 7 then raise_bad "expecting 7 columns") in
      
    let org_ver_chr = String.nsplit (to_string 4) ":" in
    let _ = (if List.length org_ver_chr <> 2 then raise_bad "expecting exactly one colon in Seq column") in
    let org = List.nth org_ver_chr 0 in
      
    let ver_chr = String.nsplit (List.nth org_ver_chr 1) ";" in
    let _ = (if List.length org_ver_chr <> 2 then raise_bad "expecting exactly one semicolon in Seq column") in
    let ver = List.nth ver_chr 0 in
    let chr = List.nth ver_chr 1 in
      {
        pmcoord = to_int 0, to_int 1;
        mmcoord = to_int 2, to_int 3;
        probe =
          {
            org_name = org;
            version = ver;
            chr_name = chr;
            start_pos = to_int 5;
            sequence =
              try Seq.of_string_exn (to_string 6)
              with Seq.Bad m -> raise_bad m
          }
      }

  let bpmap file =
    let parse file cin =
      let lines = Stream.map String.strip_final_cr (Stream.lines_of_channel cin) in
      let err msg = Msg.err ~pos:(Pos.fl file (Stream.count lines)) msg in
        try
          ignore (header (Stream.next lines));
          Stream.to_list (Stream.map row lines)
        with 
            Failure msg | Bad msg -> raise_bad (err msg)
    in
      try_finally (parse file) close_in (open_in file)

end
  
let of_file_exn = Parser.bpmap
let of_file file = try Some (of_file_exn file) with Bad _ -> None

let row_to_string r =
  let (pmx,pmy) = r.pmcoord in
  let (mmx,mmy) = r.mmcoord in
    String.concat "\t"
      [string_of_int pmx;
       string_of_int pmy;
       string_of_int mmx;
       string_of_int mmy;
       r.probe.org_name ^ ":" ^ r.probe.version ^ ";" ^ r.probe.chr_name;
       string_of_int (r.probe.start_pos);
       Seq.to_string (r.probe.sequence)
      ]
      
let to_file file t =
  let print cout =
    output_endline cout (String.concat "\t" col_names);
    List.iter ((output_endline cout) <<- row_to_string) t
  in
    try_finally print close_out (open_out_safe file)
