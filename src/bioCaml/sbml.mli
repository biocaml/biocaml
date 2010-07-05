(*sbml types*)

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

val store_attrs : (('a * 'b) * 'c) list -> ('b, 'c) Hashtbl.t
val dispatch : string -> 'a -> 'b -> (string * ('a -> 'b -> 'c)) list -> 'c
val parse_list :  Xmlm.input -> (string * (Xmlm.attribute list -> Xmlm.input -> 'a)) list -> 'a list
val parse_math : Xmlm.attribute list-> Xmlm.input -> sb_math
val parse_unit : Xmlm.attribute list-> Xmlm.input -> sb_unit
val parse_compartment : Xmlm.attribute list-> Xmlm.input -> sb_compartment
val parse_species : Xmlm.attribute list-> Xmlm.input -> sb_species
val parse_spreference : Xmlm.attribute list-> Xmlm.input -> sb_species_ref
val parse_parameter : Xmlm.attribute list-> Xmlm.input -> sb_parameter
val parse_klparameter : Xmlm.attribute list-> Xmlm.input -> kl_parameter
val parse_fundef : Xmlm.attribute list-> Xmlm.input -> sb_function_definition
val parse_unitdef : Xmlm.attribute list-> Xmlm.input -> sb_unit_definition
val parse_iassignment : Xmlm.attribute list-> Xmlm.input -> sb_initial_assignment
val parse_algebraic_rule : Xmlm.attribute list-> Xmlm.input -> sb_algebraic_rule
val parse_generic_rule : Xmlm.attribute list-> Xmlm.input -> sb_generic_rule
val parse_rule_list : Xmlm.input -> sb_rule list
val parse_kineticlaw : Xmlm.attribute list-> Xmlm.input -> sb_kinetic_law
val parse_reaction : Xmlm.attribute list-> Xmlm.input -> sb_reaction
val parse_eassignment : Xmlm.attribute list-> Xmlm.input -> sb_event_assignment
val parse_math_container : Xmlm.attribute list-> Xmlm.input -> sb_math_container
val parse_event : Xmlm.attribute list-> Xmlm.input -> sb_event
val parse_model : Xmlm.attribute list-> Xmlm.input -> sb_model
val in_sbml : in_channel -> sb_model
