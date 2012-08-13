open Biocaml_std

type probe = {org_name:string; version:string; chr_name:string; start_pos:int; sequence:Biocaml_seq.t}
type row = {pmcoord:int*int; mmcoord:int*int; probe:probe}

type t = row list
exception Bad of string
let raise_bad msg = raise (Bad msg)

let col_names = ["PMX";"PMY";"MMX";"MMY";"Seq";"Pos";"Probe"]
let num_probes = List.length  
let iter f l = List.iter ~f l
let fold f init l = List.fold_left ~f ~init l
let to_list t = t

module Parser = struct
  let header (s:string) : string list =
    let sl = String.split s '\t' in
      if sl = col_names then sl
      else raise_bad "incorrectly formatted header"

  let row ~chr_map (s:string) : row =
    match String.split s '\t' with
    | [pmcx; pmcy; mmcx; mmcy; org_ver_chr; pos; seq] ->
       let org, ver_chr =
         match String.split org_ver_chr ':' with
         | [o; ver_chr] -> o, ver_chr
         | _ -> raise_bad "expecting exactly one colon in Seq column" in
       let ver, chr =
         match String.split ver_chr ';' with
         | [v; c] -> v, chr_map c
         | _ -> raise_bad "expecting exactly one semicolon in Seq column" in
      {
        pmcoord = int_of_string pmcx, int_of_string pmcy;
        mmcoord = int_of_string mmcx, int_of_string mmcy;
        probe =
          {
            org_name = org;
            version = ver;
            chr_name = chr;
            start_pos = int_of_string pos;
            sequence = try Biocaml_seq.of_string seq
                       with Biocaml_seq.Bad m -> raise_bad m
          }
      }
    | _ -> raise_bad "expecting 7 columns"

  let bpmap ~chr_map file =
    let parse file cin =
      let lines = Stream.map String.strip_final_cr (Stream.lines_of_channel cin) in
      let err msg = Msg.err ~pos:(Pos.fl file (Stream.count lines)) msg in
        try
          ignore (header (Stream.next lines));
          Stream.to_list (Stream.map (row ~chr_map) lines)
        with 
            Failure msg | Bad msg -> raise_bad (err msg)
    in
    try_finally_exn (parse file) ~fend:close_in (open_in file)

end
  
let of_file ?(chr_map=identity) file = Parser.bpmap ~chr_map file

let row_to_string r =
  let (pmx,pmy) = r.pmcoord in
  let (mmx,mmy) = r.mmcoord in
    String.concat ~sep:"\t"
      [string_of_int pmx;
       string_of_int pmy;
       string_of_int mmx;
       string_of_int mmy;
       r.probe.org_name ^ ":" ^ r.probe.version ^ ";" ^ r.probe.chr_name;
       string_of_int (r.probe.start_pos);
       Biocaml_seq.to_string (r.probe.sequence)
      ]
      
let to_file file t =
  let print cout =
    output_endline cout (String.concat ~sep:"\t" col_names);
    List.iter ((output_endline cout) <<- row_to_string) t
  in
  try_finally_exn print ~fend:close_out (open_out_safe file)
