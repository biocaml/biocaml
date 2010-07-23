(*A module for parsing SBML level 2 version 4*)

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
 unitdef_name: string;
 unitlist: sb_unit list;
}

type sb_compartment = {
 compart_id: string;
 compart_name: string;
 spatialDimensions: int;
 size: float;
 compart_units: string;
 compart_outside: string;
 compart_constant: bool;
}

type sb_species_ref = {
 specref_species: string;
 specref_id: string;
 specref_name: string;
 stoichiometry: int;      (* TODO variant stoichiometry | stoichiometryMath *)
}

type sb_species = {
 species_id: string;
 species_name: string;
 speciesType: string;
 compartment: string;
 initialAmount: float;
 initialConcentration: float;
 substanceUnits: string;
 hasOnlySubstanceUnits: bool;
 boundaryCondition: bool;
 species_constant: bool;
}

type sb_parameter = {
 param_id: string;
 param_name: string;
 param_value: float;
 param_units: string;
 param_constant: bool;
}

type sb_kinetic_law = {
 klaw_math: sb_math;
 klaw_parameters: sb_parameter list;
}

type sb_reaction = {
 react_id: string;
 react_name: string;
 reversible: bool;
 fast: bool;
 reactants: sb_species_ref list;
 products: sb_species_ref list;
 kinetic_law: sb_kinetic_law;
}

type sb_initial_assignment = {
 ia_symbol: string;
 ia_math: sb_math;
}

type sb_algebraic_rule = {
 ar_math: sb_math;
}

type sb_generic_rule = {
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
 useValuesFromTriggerTime: bool;
 trigger: sb_trigger;
 delay: sb_delay;
 eventAssignments: sb_event_assignment list;
}

(*a wrapper type to deal with heterogenous lists*)
type sb_L = LFunctionDefinition of sb_function_definition | LUnitDefinition of sb_unit_definition | 
                   LCompartment of sb_compartment | LSpecies of sb_species | LReaction of sb_reaction | 
                   LParameter of sb_parameter | LInitialAssignment of sb_initial_assignment | LRule of sb_rule | 
                   LEvent of sb_event | LEventAssignment of sb_event_assignment | LSpecieRef of sb_species_ref |
                   LUnit of sb_unit 

type sb_model = { 
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

let parse_list i assoclist = 
 let rec iter_list i templist = 
    match Xmlm.input i with 
    | `El_start ((_, tagname), attrs) -> iter_list i ((try ((List.assoc tagname assoclist) attrs i) with Not_found -> invalid_arg tagname) :: templist)
    | `El_end -> templist
    | `Data dat -> iter_list i templist
    | `Dtd _ -> assert false
 in 
 iter_list i []

let parse_record i list_dict record_dict =
 let list_hash = Hashtbl.create 10 in
 let record_hash = Hashtbl.create 10 in 
 let rec iter_record i = 
    match Xmlm.input i with 
    | `El_start ((_, tagname), attrs) -> (if (String.compare (String.sub tagname 0 4) "list")=0 
                           then (try (Hashtbl.add list_hash tagname (parse_list i (List.assoc tagname list_dict))) with Not_found -> invalid_arg tagname)
                           else (try (Hashtbl.add record_hash tagname ((List.assoc tagname record_dict) attrs i)) with Not_found -> invalid_arg tagname)); 
                                         iter_record i 
    | `El_end -> ()
    | `Data dat -> iter_record i 
    | `Dtd _ -> assert false
 in iter_record i; (list_hash, record_hash)

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
 LUnit ({
  kind = (Hashtbl.find parse_hash "kind"); 
  exponent = (try (int_of_string (Hashtbl.find parse_hash "exponent")) with Not_found -> 1);
  scale = (try int_of_string (Hashtbl.find parse_hash "scale") with Not_found -> 0);
  multiplier = (try (float_of_string (Hashtbl.find parse_hash "multiplier")) with Not_found -> 1.0)
 })

let parse_compartment attrs i = 
 ignore (Xmlm.input i); let parse_hash = (store_attrs attrs) in 
 LCompartment ({
  compart_id = (Hashtbl.find parse_hash "id");
  compart_name = (try (Hashtbl.find parse_hash "name") with Not_found -> "");
  size = (try (float_of_string (Hashtbl.find parse_hash "size")) with Not_found -> 0.0);
  spatialDimensions = (try (int_of_string  (Hashtbl.find parse_hash "spatialDimensions")) with Not_found -> 3);
  compart_units = (try (Hashtbl.find parse_hash "units") with Not_found -> "");
  compart_outside = (try (Hashtbl.find parse_hash "outside") with Not_found -> "");
  compart_constant = (try (bool_of_string (Hashtbl.find parse_hash "constant")) with Not_found -> true);
 })

let parse_species attrs i = 
 ignore (Xmlm.input i); let parse_hash = (store_attrs attrs) in 
 LSpecies ({
  species_id = (Hashtbl.find parse_hash "id");
  species_name = (try (Hashtbl.find parse_hash "name") with Not_found -> "");
  speciesType = (try (Hashtbl.find parse_hash "speciesType") with Not_found -> "");
  compartment = (Hashtbl.find parse_hash "compartment");
  initialAmount = (try (float_of_string (Hashtbl.find parse_hash "initialAmount")) with Not_found -> 0.0);
  initialConcentration = (try (float_of_string (Hashtbl.find parse_hash "initialConcentration")) with Not_found -> 0.0);
  substanceUnits = (try (Hashtbl.find parse_hash "substanceUnits") with Not_found -> "");
  hasOnlySubstanceUnits = (try (bool_of_string (Hashtbl.find parse_hash "hasOnlySubstanceUnits")) with Not_found -> false);
  boundaryCondition = (try (bool_of_string (Hashtbl.find parse_hash "boundaryCondition")) with Not_found -> false);
  species_constant = (try (bool_of_string (Hashtbl.find parse_hash "constant")) with Not_found -> false);
 })

let parse_spreference attrs i = 
 ignore (Xmlm.input i); let parse_hash = (store_attrs attrs) in 
 LSpecieRef ({
  specref_species = (Hashtbl.find parse_hash "species");
  specref_id = (try (Hashtbl.find parse_hash "id") with Not_found -> "");
  specref_name = (try (Hashtbl.find parse_hash "name") with Not_found -> "");
  stoichiometry = (try (int_of_string  (Hashtbl.find parse_hash "stoichiometry")) with Not_found -> 1)
 })

let parse_parameter attrs i = 
 ignore (Xmlm.input i); let parse_hash = (store_attrs attrs) in 
 LParameter ({
  param_id = (try (Hashtbl.find parse_hash "id") with Not_found -> invalid_arg "no id for parameter") ;
  param_name = (try (Hashtbl.find parse_hash "name") with Not_found -> "");
  param_value = (try (float_of_string (Hashtbl.find parse_hash "value")) with Not_found -> 0.0);
  param_units = (try (Hashtbl.find parse_hash "units") with Not_found -> "");
  param_constant = (try (bool_of_string (Hashtbl.find parse_hash "constant")) with Not_found -> true);
 })

(*container record parsing*)

let parse_fundef attrs i = 
 let parse_hash = (store_attrs attrs) in
  let (list_hash,record_hash) = (parse_record i [] 
                                                [("math",parse_math)]) in
 LFunctionDefinition ({
  fundef_id = (Hashtbl.find parse_hash "id"); 
  fundef_name = (try (Hashtbl.find parse_hash "name") with Not_found -> "");
  fundef_math = (try (Hashtbl.find record_hash "math") with Not_found -> { xmlns = ""; apply = [] });
 })

let parse_unitdef attrs i = 
 let parse_hash = (store_attrs attrs) in
  let (list_hash,record_hash) = (parse_record i [("listOfUnits",[("unit",parse_unit)])] 
                                                []) in
 LUnitDefinition ({
  unitdef_id = (Hashtbl.find parse_hash "id");
  unitdef_name = (try (Hashtbl.find parse_hash "name") with Not_found -> ""); 
  unitlist = (try (List.rev_map (function LUnit(t) -> t | _ -> invalid_arg "unit") 
             (Hashtbl.find record_hash "listOfUnits")) with Not_found -> []);
 })

let parse_iassignment attrs i = 
let parse_hash = (store_attrs attrs) in
  let (list_hash,record_hash) = (parse_record i [] 
                                                [("math",parse_math)]) in
 LInitialAssignment ({
  ia_symbol = (Hashtbl.find parse_hash "symbol"); 
  ia_math = (try (Hashtbl.find record_hash "math") with Not_found -> { xmlns = ""; apply = [] });
 })

let parse_generic_rule attrs i = 
let parse_hash = (store_attrs attrs) in
  let (list_hash,record_hash) = (parse_record i [] 
                                                [("math",parse_math)]) in
 {
  gr_variable = (Hashtbl.find parse_hash "variable");
  gr_math = (try (Hashtbl.find record_hash "math") with Not_found -> { xmlns = ""; apply = [] });
 }
let parse_algebraic_rule attrs i = 
 let (list_hash,record_hash) = (parse_record i [] 
                                                [("math",parse_math)]) in
 LRule (AlgebraicRule ({
  ar_math = (try (Hashtbl.find record_hash "math") with Not_found -> { xmlns = ""; apply = [] });
 }))
let parse_assignment_rule attrs i = LRule (AssignmentRule (parse_generic_rule attrs i))
let parse_rate_rule attrs i = LRule (RateRule (parse_generic_rule attrs i))

let parse_kineticlaw attrs i = 
 let (list_hash,record_hash) = (parse_record i [("listOfParameters",[("parameter",parse_parameter)])] 
                                                [("math",parse_math)]) in
 {
  klaw_parameters = (try (List.rev_map (function LParameter(t) -> t | _ -> invalid_arg "parameter") 
                    (Hashtbl.find list_hash "listOfParameters")) with Not_found -> []);
  klaw_math = (try (Hashtbl.find record_hash "math") with Not_found -> { xmlns = ""; apply = [] });
 }

let parse_reaction attrs i = 
 let parse_hash = (store_attrs attrs) in
  let (list_hash,record_hash) = (parse_record i [("listOfReactants",[("speciesReference",parse_spreference)]);
                                                 ("listOfProducts",[("speciesReference",parse_spreference)])] 
                                                [("kineticLaw",parse_kineticlaw)]) in
 LReaction ({
  react_id = (Hashtbl.find parse_hash "id");
  react_name = (try (Hashtbl.find parse_hash "name") with Not_found -> "");
  reversible = (try (bool_of_string (Hashtbl.find parse_hash "reversible")) with Not_found -> true);
  fast = (try (bool_of_string (Hashtbl.find parse_hash "fast")) with Not_found -> false);
  reactants = (try (List.rev_map (function LSpecieRef(t) -> t | _ -> invalid_arg "specref") 
              (Hashtbl.find list_hash "listOfReactants")) with Not_found -> []);
  products = (try (List.rev_map (function LSpecieRef(t) -> t | _ -> invalid_arg "specref")
             (Hashtbl.find list_hash "listOfProducts")) with Not_found -> []);
  kinetic_law = (try (Hashtbl.find record_hash "kineticLaw") 
        with Not_found -> {klaw_parameters = []; klaw_math = { xmlns = ""; apply = [] }});
 })

let parse_eassignment attrs i = 
let parse_hash = (store_attrs attrs) in
  let (list_hash,record_hash) = (parse_record i [] 
                                                [("math",parse_math)]) in
 LEventAssignment ({
  ea_variable = (Hashtbl.find parse_hash "variable");
  ea_math = (try (Hashtbl.find record_hash "math") with Not_found -> { xmlns = ""; apply = [] });
 })

let parse_math_container attrs i = 
 let (list_hash,record_hash) = (parse_record i [] 
                                                [("math",parse_math)]) in
 {
  math = (try (Hashtbl.find record_hash "math") with Not_found -> { xmlns = ""; apply = [] });
 }

let parse_event attrs i = 
 let parse_hash = (store_attrs attrs) in
  let (list_hash,record_hash) = (parse_record i [("listOfEventAssignments",[("eventAssignment",parse_eassignment)])] 
                                                [("trigger",parse_math_container);
                                                 ("delay",parse_math_container)]) in
 LEvent ({
  event_id = (try (Hashtbl.find parse_hash "id") with Not_found -> "");
  event_name = (try (Hashtbl.find parse_hash "name") with Not_found -> "");
  useValuesFromTriggerTime = (try (bool_of_string (Hashtbl.find parse_hash "useValuesFromTriggerTime")) with Not_found -> true);
  trigger = (try Trigger (Hashtbl.find record_hash "trigger") with Not_found -> invalid_arg "trigger not found in event");
  delay = (try Delay (Hashtbl.find record_hash "delay") with Not_found -> Delay ({math = { xmlns = ""; apply = [] }}));
  eventAssignments = (try (List.rev_map (function LEventAssignment(t) -> t | _ -> invalid_arg "eassign") 
                          (Hashtbl.find list_hash "listOfEventAssignments")) with Not_found -> []);
 })

let parse_model attrs i = 
 let parse_hash = (store_attrs attrs) in
  let (list_hash,record_hash) = (parse_record i [("listOfFunctionDefinitions",[("functionDefinition",parse_fundef)]);
                                                 ("listOfUnitDefinitions",[("unitDefinition",parse_unitdef)]);
                                                 ("listOfCompartments",[("compartment",parse_compartment)]);
                                                 ("listOfSpecies",[("species",parse_species)]);
                                                 ("listOfReactions",[("reaction",parse_reaction)]);
                                                 ("listOfParameters",[("parameter",parse_parameter)]);
                                                 ("listOfInitialAssignments",[("initialAssignment",parse_iassignment)]);
                                                 ("listOfRules",[("assignmentRule",parse_assignment_rule);
                                                                      ("rateRule",parse_rate_rule);
                                                                      ("algebraicRule",parse_algebraic_rule)]);
                                                 ("listOfEvents",[("event",parse_event)])] 
                                                []) in
{ 
  model_id = (try (Hashtbl.find parse_hash "id") with Not_found -> "");
  model_name = (try (Hashtbl.find parse_hash "name") with Not_found -> "");
  functionDefinitions = (try (List.rev_map (function LFunctionDefinition(t) -> t | _ -> invalid_arg "fundef") 
                        (Hashtbl.find list_hash "listOfFunctionDefinitions")) with Not_found -> []);
  unitDefinitions = (try (List.rev_map (function LUnitDefinition(t) -> t | _ -> invalid_arg "fundef") 
                    (Hashtbl.find list_hash "listOfUnitDefinitions")) with Not_found -> []);
  compartments = (try (List.rev_map (function LCompartment(t) -> t | _ -> invalid_arg "fundef") 
                 (Hashtbl.find list_hash "listOfCompartments")) with Not_found -> []);
  species = (try (List.rev_map (function LSpecies(t) -> t | _ -> invalid_arg "fundef") 
                 (Hashtbl.find list_hash "listOfSpecies")) with Not_found -> []);
  reactions = (try (List.rev_map (function LReaction(t) -> t | _ -> invalid_arg "fundef") 
                   (Hashtbl.find list_hash "listOfReactions")) with Not_found -> []);
  parameters = (try (List.rev_map (function LParameter(t) -> t | _ -> invalid_arg "fundef") 
                    (Hashtbl.find list_hash "listOfParameters")) with Not_found -> []);
  initialAssignments = (try (List.rev_map (function LInitialAssignment(t) -> t | _ -> invalid_arg "fundef") 
                       (Hashtbl.find list_hash "listOfInitialAssignments")) with Not_found -> []);
  rules = (try (List.rev_map (function LRule(t) -> t | _ -> invalid_arg "fundef")               
          (Hashtbl.find list_hash "listOfRules")) with Not_found -> []);
  events = (try (List.rev_map (function LEvent(t) -> t | _ -> invalid_arg "fundef") 
           (Hashtbl.find list_hash "listOfEvents")) with Not_found -> []);
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
