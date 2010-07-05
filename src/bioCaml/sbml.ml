(*TODO mathml*)
type sb_math = {
 xmlns: string;
 apply: string list;
}

type sb_unit = {
 kind: string;
 exponent: int;
 scale: int;
 multiplier: float;
}

type sb_function_definition = {
 fundef_id: string;
 fundef_name: string;
 fundef_math: sb_math;
}

type sb_unit_definition = {
 unitdef_id: string;
 unitlist: sb_unit list;
}

type sb_compartment = {
 compart_id: string;
 size: float;
 spatialDimensions: int;
 compart_name: string;
}

type sb_species_ref = {
 specref_id: string;
 stoichiometry: int;
}

type sb_species = {
 species_id: string;
 compartment: string;
 initialAmount: float;
 species_name: string;
 substanceUnits: string;
 boundaryCondition: bool;
}

type kl_parameter = {
 klparam_id: string;
 klparam_value: float;
 units: string;
}

type sb_kinetic_law = {
 formula: string;
 klaw_math: sb_math;
 klaw_parameters: kl_parameter list;
}

type sb_reaction = {
 react_id: string;
 reversible: bool;
 fast: bool;
 reactants: sb_species_ref list;
 products: sb_species_ref list;
 kinetic_law: sb_kinetic_law;
}

type sb_parameter = {
 param_id: string;
 param_name: string;
 value: float;
 constant: bool;
}

type sb_initial_assignment = {
 ia_symbol: string;
 ia_math: sb_math;
}

type sb_algebraic_rule = {
 ar_metaid: string;
 ar_math: sb_math;
}

type sb_generic_rule = {
 gr_metaid: string;
 gr_variable: string;
 gr_math: sb_math;
}

type sb_rule = RateRule of sb_generic_rule | AssignmentRule of sb_generic_rule | AlgebraicRule of sb_algebraic_rule

type sb_math_container = {
 math: sb_math;
}

type sb_delay = Delay of sb_math_container
type sb_trigger = Trigger of sb_math_container  

type sb_event_assignment = {
 ea_variable: string;
 ea_math: sb_math;
}

type sb_event = {
 event_id: string;
 event_name: string;
 trigger: sb_trigger;
 delay: sb_delay;
 eventAssignments: sb_event_assignment list;
}

type sb_model = { 
 model_metaid: string;
 model_id: string;
 model_name: string;
 functionDefinitions : sb_function_definition list;
 unitDefinitions : sb_unit_definition list;
 compartments : sb_compartment list;
 species : sb_species list;
 reactions : sb_reaction list;

 parameters : sb_parameter list;
 initialAssignments : sb_initial_assignment list;
 rules : sb_rule list;
 events : sb_event list;

 (*could not find test xmls for these*)

 (*constraints : sb_constraint list;
 compartmentTypes : sb_compartment_type list;
 speciesTypes : sb_species_type list;*)
}     

(*abstract stuff for lists and attributes*)
let store_attrs attrs = 
 let parse_hash = Hashtbl.create 10 in
  let store_attr attr = 
    match attr with
    | ((_, nam), value) -> Hashtbl.add parse_hash nam value
  in List.iter store_attr attrs; parse_hash

let dispatch name attrs i assoclist =
 try ((List.assoc name assoclist) attrs i) with Not_found -> invalid_arg name 

let parse_list i assoclist = 
 let rec iter_list i templist = 
    match Xmlm.input i with 
    | `El_start ((_, tagname), attrs) -> iter_list i ((dispatch tagname attrs i assoclist) :: templist)
    | `El_end -> templist
    | `Data dat -> iter_list i templist
    | `Dtd _ -> assert false
 in iter_list i []

(*TODO mathml*)
let parse_math attrs i =
  let rec pull i depth = 
    match Xmlm.input i with 
    | `El_start ((_, loc), attrs) -> (* print_string loc; *) pull i (depth + 1)
    | `El_end -> if depth = 0 then () else pull i (depth - 1)
    | `Data dat -> (* print_endline dat; *) pull i depth 
    | `Dtd _ -> assert false
  in
  pull i 0; { xmlns = ""; apply = [] }

(*enitity record parsing, ignore (Xmlm.input i) is for skipping tag's end *)

let parse_unit attrs i =
 ignore (Xmlm.input i); let parse_hash = (store_attrs attrs) in 
 {
  kind = (Hashtbl.find parse_hash "kind"); 
  exponent = (try (int_of_string (Hashtbl.find parse_hash "exponent")) with Not_found -> 1);
  scale = (try int_of_string (Hashtbl.find parse_hash "scale") with Not_found -> 0);
  multiplier = (try (float_of_string (Hashtbl.find parse_hash "multiplier")) with Not_found -> 1.0)
 }

let parse_compartment attrs i = 
 ignore (Xmlm.input i); let parse_hash = (store_attrs attrs) in 
 {
  compart_id = (Hashtbl.find parse_hash "id");
  size = (try (float_of_string (Hashtbl.find parse_hash "size")) with Not_found -> 0.0);
  spatialDimensions = (try (int_of_string  (Hashtbl.find parse_hash "spatialDimensions")) with Not_found -> 3);
  compart_name = (try (Hashtbl.find parse_hash "name") with Not_found -> "")
 }

let parse_species attrs i = 
 ignore (Xmlm.input i); let parse_hash = (store_attrs attrs) in 
 {
  species_id = (Hashtbl.find parse_hash "id");
  compartment = (Hashtbl.find parse_hash "compartment");
  initialAmount = (try (float_of_string (Hashtbl.find parse_hash "size")) with Not_found -> 0.0);
  species_name = (try (Hashtbl.find parse_hash "name") with Not_found -> "");
  substanceUnits = (try (Hashtbl.find parse_hash "name") with Not_found -> "");
  boundaryCondition = (try (bool_of_string (Hashtbl.find parse_hash "size")) with Not_found -> false);
 }

let parse_spreference attrs i = 
 ignore (Xmlm.input i); let parse_hash = (store_attrs attrs) in 
 {
  specref_id = (Hashtbl.find parse_hash "species");
  stoichiometry = (try (int_of_string  (Hashtbl.find parse_hash "stoichiometry")) with Not_found -> 0)
 }

let parse_parameter attrs i = 
 ignore (Xmlm.input i); let parse_hash = (store_attrs attrs) in 
 {
  param_id = (Hashtbl.find parse_hash "id");
  param_name = (Hashtbl.find parse_hash "name");
  value = (try (float_of_string (Hashtbl.find parse_hash "value")) with Not_found -> 0.0);
  constant = (try (bool_of_string (Hashtbl.find parse_hash "constant")) with Not_found -> true);
 }

let parse_klparameter attrs i = 
 ignore (Xmlm.input i); let parse_hash = (store_attrs attrs) in 
 {
  klparam_id = (Hashtbl.find parse_hash "id");
  klparam_value = (try (float_of_string (Hashtbl.find parse_hash "value")) with Not_found -> 0.0);
  units = (Hashtbl.find parse_hash "units");
 }

(*container record parsing*)

let parse_fundef attrs i = 
let temp_math = ref { xmlns = ""; apply = [] } in
 let parse_hash = (store_attrs attrs) in
 let rec iter_record i = 
   match Xmlm.input i with 
    | `El_start ((_, "math"), attrs) -> temp_math := parse_math attrs i; iter_record i 
    | `El_start ((_, name), _) -> invalid_arg name
    | `El_end -> ()
    | `Data dat -> iter_record i
    | `Dtd _ -> assert false
 in iter_record i; 
 {
  fundef_id = (Hashtbl.find parse_hash "id"); 
  fundef_name = (Hashtbl.find parse_hash "name"); 
  fundef_math = !temp_math;
 }

let parse_unitdef attrs i = 
 let temp_list = ref [] in
 let rec iter_record i  = 
   match Xmlm.input i with 
    | `El_start ((_, "listOfUnits"), attrs) -> temp_list := (parse_list i [("unit",parse_unit)]); iter_record i 
    | `El_start ((_, name), attrs) -> invalid_arg name
    | `El_end -> ()
    | `Data dat -> iter_record i
    | `Dtd _ -> assert false
 in iter_record i; 
 let parse_hash = (store_attrs attrs) in
 {
  unitdef_id = (Hashtbl.find parse_hash "id"); 
  unitlist = !temp_list
 }

let parse_iassignment attrs i = 
let temp_math = ref { xmlns = ""; apply = [] } in
 let parse_hash = (store_attrs attrs) in
 let rec iter_record i = 
   match Xmlm.input i with 
    | `El_start ((_, "math"), attrs) -> temp_math := parse_math attrs i; iter_record i 
    | `El_start ((_, name), _) -> invalid_arg name
    | `El_end -> ()
    | `Data dat -> iter_record i
    | `Dtd _ -> assert false
 in iter_record i; 
 {
  ia_symbol = (Hashtbl.find parse_hash "symbol"); 
  ia_math = !temp_math;
 }

let parse_algebraic_rule attrs i = 
let temp_math = ref { xmlns = ""; apply = [] } in
 let parse_hash = (store_attrs attrs) in
 let rec iter_record i = 
   match Xmlm.input i with 
    | `El_start ((_, "math"), attrs) -> temp_math := parse_math attrs i; iter_record i 
    | `El_start ((_, name), _) -> invalid_arg name
    | `El_end -> ()
    | `Data dat -> iter_record i
    | `Dtd _ -> assert false
 in iter_record i; 
 {
  ar_metaid = (Hashtbl.find parse_hash "metaid");
  ar_math = !temp_math;
 }

let parse_generic_rule attrs i = 
let temp_math = ref { xmlns = ""; apply = [] } in
 let parse_hash = (store_attrs attrs) in
 let rec iter_record i = 
   match Xmlm.input i with 
    | `El_start ((_, "math"), attrs) -> temp_math := parse_math attrs i; iter_record i 
    | `El_start ((_, name), _) -> invalid_arg name
    | `El_end -> ()
    | `Data dat -> iter_record i
    | `Dtd _ -> assert false
 in iter_record i; 
 {
 gr_metaid = (Hashtbl.find parse_hash "metaid");
 gr_variable = (Hashtbl.find parse_hash "variable");
 gr_math = !temp_math;
}

let parse_rule_list i =
 let rec iter_list i templist = 
    match Xmlm.input i with 
    | `El_start ((_, "assignmentRule"), attrs) -> iter_list i (AssignmentRule (parse_generic_rule attrs i) :: templist)
    | `El_start ((_, "rateRule"), attrs) -> iter_list i (RateRule (parse_generic_rule attrs i) :: templist)
    | `El_start ((_, "algebraicRule"), attrs) -> iter_list i (AlgebraicRule (parse_algebraic_rule attrs i) :: templist)
    | `El_start ((_, name), _) -> invalid_arg name
    | `El_end -> templist
    | `Data dat -> iter_list i templist
    | `Dtd _ -> assert false
 in iter_list i []

let parse_kineticlaw attrs i = 
let temp_param = ref [] and
    temp_math = ref { xmlns = ""; apply = [] } in
 let parse_hash = (store_attrs attrs) in
 let rec iter_record i = 
   match Xmlm.input i with 
    | `El_start ((_, "listOfParameters"), _) -> temp_param := (parse_list i [("parameter",parse_klparameter)]); iter_record i
    | `El_start ((_, "math"), attrs) -> temp_math := parse_math attrs i; iter_record i
    | `El_start ((_, name), _) -> invalid_arg name
    | `El_end -> ()
    | `Data dat -> iter_record i
    | `Dtd _ -> assert false
 in iter_record i; 
 {
  formula = (try (Hashtbl.find parse_hash "formula") with Not_found -> "");  
  klaw_parameters = !temp_param;
  klaw_math = !temp_math;
 }

let parse_reaction attrs i = 
 let temp_rl = ref [] and
     temp_pl = ref [] and
     temp_klaw = ref {formula = ""; klaw_parameters = []; klaw_math = { xmlns = ""; apply = [] }} in
 let parse_hash = (store_attrs attrs) in
 let rec iter_record i = 
    match Xmlm.input i with 
    | `El_start ((_, "listOfReactants"), _) -> temp_rl := (parse_list i [("speciesReference",parse_spreference)]); iter_record i
    | `El_start ((_, "listOfProducts"), _) -> temp_pl := (parse_list i [("speciesReference",parse_spreference)]); iter_record i
    | `El_start ((_, "kineticLaw"), attrs) -> temp_klaw := parse_kineticlaw attrs i; iter_record i
    | `El_start ((_, name), _) -> invalid_arg name
    | `El_end -> ()
    | `Data dat -> iter_record i 
    | `Dtd _ -> assert false
 in iter_record i; 
 {
  react_id = (try (Hashtbl.find parse_hash "id") with Not_found -> "");
  reversible = (try (bool_of_string (Hashtbl.find parse_hash "reversible")) with Not_found -> false);
  fast = (try (bool_of_string (Hashtbl.find parse_hash "fast")) with Not_found -> false);
  reactants = !temp_rl;
  products = !temp_pl;
  kinetic_law = !temp_klaw;
 }

let parse_eassignment attrs i = 
let temp_math = ref { xmlns = ""; apply = [] } in
 let parse_hash = (store_attrs attrs) in
 let rec iter_record i = 
   match Xmlm.input i with 
    | `El_start ((_, "math"), attrs) -> temp_math := parse_math attrs i; iter_record i
    | `El_start ((_, name), _) -> invalid_arg name
    | `El_end -> ()
    | `Data dat -> iter_record i
    | `Dtd _ -> assert false
 in iter_record i; 
 {
  ea_variable = (Hashtbl.find parse_hash "variable");
  ea_math = !temp_math;
 }

let parse_math_container attrs i = 
let temp_math = ref { xmlns = ""; apply = [] } in
 let rec iter_record i = 
   match Xmlm.input i with 
    | `El_start ((_, "math"), attrs) -> temp_math := parse_math attrs i; iter_record i 
    | `El_start ((_, name), _) -> invalid_arg name
    | `El_end -> ()
    | `Data dat -> iter_record i
    | `Dtd _ -> assert false
 in iter_record i; 
 {
  math = !temp_math;
 }

let parse_event attrs i = 
let temp_eal = ref [] and
    temp_trigger = ref (Trigger ({math = { xmlns = ""; apply = [] }})) and
    temp_delay = ref (Delay ({math = { xmlns = ""; apply = [] }})) in
 let parse_hash = (store_attrs attrs) in
 let rec iter_record i = 
   match Xmlm.input i with 
    | `El_start ((_, "listOfEventAssignments"), _) -> temp_eal := (parse_list i [("eventAssignment",parse_eassignment)]); iter_record i
    | `El_start ((_, "trigger"), attrs) -> temp_trigger := Trigger (parse_math_container attrs i); iter_record i
    | `El_start ((_, "delay"), attrs) -> temp_delay := Delay (parse_math_container attrs i); iter_record i
    | `El_start ((_, name), _) -> invalid_arg name
    | `El_end -> ()
    | `Data dat -> iter_record i
    | `Dtd _ -> assert false
 in iter_record i; 
 {
  event_id = (try (Hashtbl.find parse_hash "id") with Not_found -> "");
  event_name = (try (Hashtbl.find parse_hash "name") with Not_found -> "");
  trigger = !temp_trigger; (*TODO make not optional!*)
  delay = !temp_delay;
  eventAssignments = !temp_eal;
 }

let parse_model attrs i = 
 let temp_udl = ref [] and
     temp_fdl = ref [] and 
     temp_cl = ref [] and
     temp_sl = ref [] and
     temp_real = ref [] and
     temp_pl = ref [] and
     temp_ial = ref [] and
     temp_rul = ref [] and
     temp_el = ref [] in
 let parse_hash = (store_attrs attrs) in
 let rec pull i = 
    match Xmlm.input i with 
    | `El_start ((_, "listOfFunctionDefinitions"), _) -> temp_fdl := (parse_list i [("functionDefinition",parse_fundef)]); pull i
    | `El_start ((_, "listOfUnitDefinitions"), _) -> temp_udl := (parse_list i [("unitDefinition",parse_unitdef)]); pull i
    | `El_start ((_, "listOfCompartments"), _) -> temp_cl := (parse_list i [("compartment",parse_compartment)]); pull i 
    | `El_start ((_, "listOfSpecies"), _) -> temp_sl := (parse_list i [("species",parse_species)]); pull i 
    | `El_start ((_, "listOfReactions"), _) -> temp_real := (parse_list i [("reaction",parse_reaction)]); pull i    
    | `El_start ((_, "listOfParameters"), _) -> temp_pl := (parse_list i [("parameter",parse_parameter)]); pull i
    | `El_start ((_, "listOfInitialAssignments"), _) -> temp_ial := (parse_list i [("initialAssignment",parse_iassignment)]); pull i
    | `El_start ((_, "listOfRules"), _) -> temp_rul := (parse_rule_list i); pull i
    | `El_start ((_, "listOfEvents"), _) -> temp_el := (parse_list i [("event",parse_event)]); pull i
    | `El_start ((_, loc), _) -> invalid_arg loc
    | `El_end -> ()
    | `Data dat -> pull i 
    | `Dtd _ -> assert false
 in pull i;
 { 
  model_metaid = (try (Hashtbl.find parse_hash "metaid") with Not_found -> "");
  model_id = (try (Hashtbl.find parse_hash "id") with Not_found -> "");
  model_name = (try (Hashtbl.find parse_hash "name") with Not_found -> "");
  functionDefinitions = !temp_fdl;
  unitDefinitions = !temp_udl;
  compartments = !temp_cl;
  species = !temp_sl;
  reactions = !temp_real;
  parameters = !temp_pl;
  initialAssignments = !temp_ial;
  rules = !temp_rul;
  events = !temp_el;
 }  

(*reader function*)
let in_sbml ichan =
  let i = (Xmlm.make_input (`Channel ichan)) in
  ignore (Xmlm.input i); (* `Dtd *)
  ignore (Xmlm.input i); (* smbl tag start *)
  ignore (Xmlm.input i); (* smbl tag bogus data *)
  let model =
   match Xmlm.input i with 
   | `El_start ((_, "model"), attrs) -> parse_model attrs i
   | _ -> invalid_arg "document not well-formed" in
  ignore (Xmlm.input i); (* smbl tag bogus data *)
  ignore (Xmlm.input i); (* smbl tag end *)
  if not (Xmlm.eoi i) then invalid_arg "document not well-formed";
  model
