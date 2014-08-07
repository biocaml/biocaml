(** COSMIC files. *)
open Core.Std

let lines_of_file (file : string) : string Sequence.t =
  let cin = In_channel.create file in
  Sequence.unfold ~init:() ~f:(fun () ->
    match In_channel.input_line cin with
    | None -> (In_channel.close cin; None)
    | Some s -> Some (s, ())
  )

type item  = {
  cosmic_mart_pk_key : int;
  cosmic_id_sample : int;
  cosmic_id_gene : int option;
  sample_name : string;
  gene_name : string;
  entrez_gene_id : int option;
  ensg_id : string;
  swissprot_id : string;
  accession_number : string;
  cosmic_id_mutation : string option;
  cosmic_id_fusion_mutation : string option;
  mut_type_cds : string option;
  cds_mut_syntax : string option;
  cds_mut_start : string option;
  cds_mut_stop : string option;
  mut_type_aa : string option;
  aa_mut_syntax : string option;
  aa_mut_start : string option;
  aa_mut_stop : string option;
  ncbi36_coords : string option;
  grch37_coords : string option;
  zygosity : string;
  in_cancer_census : string;
  samp_gene_mutated : string;
  somatic_status : string;
  validation_status : string;
  whole_genome_screen : string;
  site_primary : string;
  site_subtype_1 : string;
  site_subtype_2 : string;
  site_subtype_3 : string;
  hist_primary : string;
  hist_subtype_1 : string;
  hist_subtype_2 : string;
  hist_subtype_3 : string;
  sample_source : string;
  tumour_source : string;
  pubmed_pmid : int option;
  cosmic_id_study : int option}

type something_in_cosmic_file =
| Item of item
| Something of string
| Header of string
| Empty

let parse_line (line_num : int) (cosmic_line : string)
 : something_in_cosmic_file =
  let header =
    "COSMIC_MART_PK_KEY,COSMIC_ID_SAMPLE,COSMIC_ID_GENE,SAMPLE_NAME,GENE_NAME,ENTREZ_GENE_ID,ENSG_ID,SWISSPROT_ID,ACCESSION_NUMBER,COSMIC_ID_MUTATION,COSMIC_ID_FUSION_MUTATION,MUT_TYPE_CDS,CDS_MUT_SYNTAX,CDS_MUT_START,CDS_MUT_STOP,MUT_TYPE_AA,AA_MUT_SYNTAX,AA_MUT_START,AA_MUT_STOP,NCBI36_COORDS,GRCH37_COORDS,ZYGOSITY,IN_CANCER_CENSUS,SAMP_GENE_MUTATED,SOMATIC_STATUS,VALIDATION_STATUS,WHOLE_GENOME_SCREEN,SITE_PRIMARY,SITE_SUBTYPE_1,SITE_SUBTYPE_2,SITE_SUBTYPE_3,HIST_PRIMARY,HIST_SUBTYPE_1,HIST_SUBTYPE_2,HIST_SUBTYPE_3,SAMPLE_SOURCE,TUMOUR_SOURCE,PUBMED_PMID,COSMIC_ID_STUDY" in
  if cosmic_line = "" then
    Empty
  else if cosmic_line = "7410326 rows selected." then
    Something "The last line exists"
  else if cosmic_line = header then
    Header "There is a header present that matches the input of my header"
  else
    match String.split cosmic_line ~on:',' with
    | [cosmic_mart_pk_key; cosmic_id_sample; cosmic_id_gene; sample_name;
       gene_name; entrez_gene_id; ensg_id; swissprot_id; accession_number;
       cosmic_id_mutation; cosmic_id_fusion_mutation; mut_type_cds;
       cds_mut_syntax; cds_mut_start; cds_mut_stop; mut_type_aa;
       aa_mut_syntax; aa_mut_start; aa_mut_stop; ncbi36_coords;
       grch37_coords; zygosity; in_cancer_census; samp_gene_mutated;
       somatic_status; validation_status; whole_genome_screen;
       site_primary; site_subtype_1; site_subtype_2; site_subtype_3;
       hist_primary; hist_subtype_1; hist_subtype_2; hist_subtype_3;
       sample_source; tumour_source; pubmed_pmid; cosmic_id_study
      ] -> Item {
      cosmic_mart_pk_key = Int.of_string cosmic_mart_pk_key;
      cosmic_id_sample = Int.of_string cosmic_id_sample;
      cosmic_id_gene =(
        if cosmic_id_gene = ""
        then None
        else Some (Int.of_string cosmic_id_gene)
      );
      sample_name;
      gene_name;
      entrez_gene_id =(
        if entrez_gene_id = ""
        then None
        else Some (Int.of_string entrez_gene_id)
      );
      ensg_id;
      swissprot_id;
      accession_number;
      cosmic_id_mutation =(
        if cosmic_id_mutation = ""
        then None
        else Some (cosmic_id_mutation)
      );
      cosmic_id_fusion_mutation =(
        if cosmic_id_fusion_mutation = ""
        then None
        else Some (cosmic_id_fusion_mutation)
      );
      mut_type_cds =(
        if mut_type_cds = ""
        then None
        else Some (mut_type_cds)
      );
      cds_mut_syntax =(
        if cds_mut_syntax = ""
        then None
        else Some (cds_mut_syntax)
      );
      cds_mut_start =(
        if cds_mut_start = ""
        then None
        else Some (cds_mut_start)
      );
      cds_mut_stop =(
        if cds_mut_stop = ""
        then None
        else Some (cds_mut_stop)
      );
      mut_type_aa =(
        if mut_type_aa = ""
        then None
        else Some (mut_type_aa)
      );
      aa_mut_syntax =(
        if aa_mut_syntax = ""
        then None
        else Some (aa_mut_syntax)
      );
      aa_mut_start =(
        if aa_mut_start = ""
        then None
        else Some (aa_mut_start)
      );
      aa_mut_stop =(
        if aa_mut_stop = ""
        then None
        else Some (aa_mut_stop)
      );
      ncbi36_coords =(
        if ncbi36_coords = ""
        then None
        else Some (ncbi36_coords)
      );
      grch37_coords =(
        if grch37_coords = ""
        then None
        else Some (grch37_coords)
      );
      zygosity;
      in_cancer_census;
      samp_gene_mutated;
      somatic_status;
      validation_status;
      whole_genome_screen;
      site_primary;
      site_subtype_1;
      site_subtype_2;
      site_subtype_3;
      hist_primary;
      hist_subtype_1;
      hist_subtype_2;
      hist_subtype_3;
      sample_source;
      tumour_source;
      pubmed_pmid =(
        if pubmed_pmid = ""
        then None
        else Some (Int.of_string pubmed_pmid)
      );
      cosmic_id_study =(
        if cosmic_id_study = ""
        then None
        else Some (Int.of_string cosmic_id_study)
          )
    }
    | _ -> failwithf "line %d: expected exactly 39 columns" line_num ()

let parse_file (cosmic_file : string) : (something_in_cosmic_file Sequence.t) =
  let (lines : string Sequence.t) = lines_of_file cosmic_file in
  Sequence.mapi lines ~f:parse_line

let header_agrees_with (y : string) (x : something_in_cosmic_file) : bool =
  match x
  with Header u -> u = y
  | _ -> false

let print_header (header : string) : unit =
  Printf.printf
    "%s"
    header

let get_header (input : something_in_cosmic_file)
    : string =
  match input with
  | Header z -> z
  | Something _ -> assert false
  | Item _ -> assert false
  | Empty -> assert false

let finding_the_header (cosmic_file : string) (header_to_find : string) =
  let (first_line : something_in_cosmic_file Sequence.t) =
    parse_file cosmic_file in
  let f (_ : int) (line : something_in_cosmic_file) : bool =
    header_agrees_with header_to_find line in
  let (first_line : something_in_cosmic_file Sequence.t) =
    Sequence.filteri first_line ~f in
  let (first_line : string Sequence.t) =
    Sequence.map first_line ~f:get_header in
  Sequence.iter first_line ~f:print_header

let get_item (input : something_in_cosmic_file) : item =
  match input with
  | Item z -> z
  | Something _ -> assert false
  | Header _ -> assert false
  | Empty -> assert false

let agrees_with (y : string) (x : something_in_cosmic_file) : bool =
  match x
  with Item u -> u.gene_name = y
  | _ -> false

let agrees_with_site (y : string) (x : something_in_cosmic_file) :
    bool =
  match x
  with Item u -> u.site_primary = y
  | _ -> false

let print_results (y : item) : unit =
  Printf.printf
    "%d,%d,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n"
    y.cosmic_mart_pk_key
    y.cosmic_id_sample
    (match y.cosmic_id_gene with
    | None -> ""
    | Some x -> (Int.to_string x)
    )
    y.sample_name
    y.gene_name
    (match y.entrez_gene_id with
    | None -> ""
    | Some x -> (Int.to_string x)
    )
    y.ensg_id
    y.swissprot_id
    y.accession_number
    (match y.cosmic_id_mutation with
    | None -> ""
    | Some x -> (x)
    )
    (match y.cosmic_id_fusion_mutation with
    | None -> ""
    | Some x -> (x)
    )
    (match y.mut_type_cds with
    | None -> ""
    | Some x -> (x)
    )
    (match y.cds_mut_syntax with
    | None -> ""
    | Some x -> (x)
    )
    (match y.cds_mut_start with
    | None -> ""
    | Some x -> (x)
    )
    (match y.cds_mut_stop with
    | None -> ""
    | Some x -> (x)
    )
    (match y.mut_type_aa with
    | None -> ""
    | Some x -> (x)
    )
    (match y.aa_mut_syntax with
    | None -> ""
    | Some x -> (x)
    )
    (match y.aa_mut_start with
    | None -> ""
    | Some x -> (x)
    )
    (match y.aa_mut_stop with
    | None -> ""
    | Some x -> (x)
    )
    (match y.ncbi36_coords with
    | None -> ""
    | Some x -> (x)
    )
    (match y.grch37_coords with
    | None -> ""
    | Some x -> (x)
    )
    y.zygosity
    y.in_cancer_census
    y.samp_gene_mutated
    y.somatic_status
    y.validation_status
    y.whole_genome_screen
    y.site_primary
    y.site_subtype_1
    y.site_subtype_2
    y.site_subtype_3
    y.hist_primary
    y.hist_subtype_1
    y.hist_subtype_2
    y.hist_subtype_3
    y.sample_source
    y.tumour_source
    (match y.pubmed_pmid with
    | None -> ""
    | Some x -> (Int.to_string x)
    )
    (match y.cosmic_id_study with
    | None -> ""
    | Some x -> (Int.to_string x)
    )

let application_of_agrees_with (cosmic_file : string) (input : string) =
  let lines : something_in_cosmic_file Sequence.t =
    parse_file cosmic_file in
  let f (_ : int) (line : something_in_cosmic_file) : bool =
    agrees_with_site input line in
  let (lines : something_in_cosmic_file Sequence.t) =
    Sequence.filteri lines ~f in
  let (lines : item Sequence.t) =
    Sequence.map lines ~f:get_item in
  Sequence.iter lines ~f:print_results

(*let z = application_of_agrees_with file "lung"
*)
let get_something (input : something_in_cosmic_file) : string =
  match input with
  | Something z -> z
  | Header _ -> assert false
  | Item _ -> assert false
  | Empty -> assert false

let last_line_agrees (y : string) (x : something_in_cosmic_file) : bool =
  match x
  with Something u -> u = y
  | _ -> false

let print_last_line (last_line : string) : unit =
  Printf.printf
    "%s"
    last_line

let finding_the_last_line (cosmic_file : string) (input : string) =
  let (last_line : something_in_cosmic_file Sequence.t) =
    parse_file cosmic_file in
  let f (_ : int) (line : something_in_cosmic_file) : bool =
    last_line_agrees input line in
  let (last_line : something_in_cosmic_file Sequence.t) =
    Sequence.filteri last_line ~f in
  let (last_line : string Sequence.t) =
    Sequence.map last_line ~f:get_something in
  Sequence.iter last_line ~f: print_last_line
