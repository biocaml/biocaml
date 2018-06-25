(** SBML file parser. Currently only level 2 version 4 is supported. *)

exception Bad of string

type sb_math_operator = 
 (* arithmetics *)
 | MPlus         (* a + b   *)
 | MMinus        (* a - b   *)
 | MTimes        (* a * b   *)
 | MDivide       (* a / b   *)
 | MPower        (* a ^ b   *)
 | MRoot         (* a^(1/b) *)
 | MAbs          (* |a|     *)
 | MExp          (* e^a     *)
 | MLn           (* ln a    *)
 | MLog          (* log a,b *)
 | MFloor        (* floor a *)
 | MCeiling      (* ceil a  *)
 | MFactorial    (* a!      *)
 (* relational *)
 | MEq           (* a == b *)
 | MNeq          (* a != b *)
 | MGt           (* a > b  *)
 | MLt           (* a < b  *)
 | MGeq          (* a >= b *)
 | MLeq          (* a <= b *)
 (* logic *)
 | MAnd          (* a & b  *)
 | MOr           (* a | b  *)
 | MXor          (* a ^^ b *)
 | MNot          (* !a     *)
 (*trigonometry*)
 | MSin 
 | MCos 
 | MTan 
 | MArcsin 
 | MArccos 
 | MArctan 
 (*delay a,b - see SBML spec *)
 | MDelay       
 (*user-defined functions*)
 | MFundef of string

type sb_math = 
 (* composite *)
 | MApply of sb_math_operator * (sb_math list)
 | MLambda of (string list) * sb_math
 | MPiecewise of ((string * sb_math) list) * string
 (* tokens *)
 | MFloatNumber of float
 | MIntNumber of int
 | MIdentifier of string
 | MTime         (* simulation time - see SBML spec*)
 | MTrue
 | MFalse
 | MNAN
 | MPi
 | MExponent
 | MInfinity
 | MNoMath     

type sb_unit = {
 unit_kind: string;
 unit_exponent: int;
 unit_scale: int;
 unit_multiplier: float;
}

type sb_function_definition = {
 fundef_id: string;
 fundef_name: string;
 fundef_math: sb_math;
}

type sb_unit_definition = {
 unitdef_id: string;
 unitdef_name: string;
 unitdef_unitlist: sb_unit list;
}

type sb_compartment = {
 compart_id: string;
 compart_name: string;
 compart_spatialDimensions: int;
 compart_size: float;
 compart_units: string;
 compart_outside: string;
 compart_constant: bool;
}

type sb_species_ref = {
 specref_species: string;
 specref_id: string;
 specref_name: string;
 specref_stoichiometry: int;      (* TODO variant stoichiometry | stoichiometryMath *)
}

type sb_species = {
 species_id: string;
 species_name: string;
 species_type: string;
 species_compartment: string;
 species_initialAmount: float;
 species_initialConcentration: float;
 species_substanceUnits: string;
 species_hasOnlySubstanceUnits: bool;
 species_boundaryCondition: bool;
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
 react_boundaryCondition: bool;
 react_fast: bool;
 react_reactants: sb_species_ref list;
 react_products: sb_species_ref list;
 react_kineticLaw: sb_kinetic_law;
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
 event_useValuesFromTriggerTime: bool;
 event_trigger: sb_trigger;
 event_delay: sb_delay;
 event_assignments: sb_event_assignment list;
}

type sb_model = { 
 sbm_id: string;
 sbm_name: string;
 sbm_functionDefinitions : sb_function_definition list;
 sbm_unitDefinitions : sb_unit_definition list;
 sbm_compartments : sb_compartment list;
 sbm_species : sb_species list;
 sbm_reactions : sb_reaction list;
 sbm_parameters : sb_parameter list;
 sbm_initialAssignments : sb_initial_assignment list;
 sbm_rules : sb_rule list;
 sbm_events : sb_event list;

 (*could not find test xmls for these*)

 (*constraints : sb_constraint list;
 compartmentTypes : sb_compartment_type list;
 speciesTypes : sb_species_type list;*)
}

val math_to_string : sb_math -> string
  (** Returns a string with sb_math converted into a S-expression  *)

val in_sbml : In_channel.t -> sb_model
  (** Returns an sb_model read from input stream *)
