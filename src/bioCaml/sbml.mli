(** SBML file parser. Currently only level 2 version 4 is supported. *)

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

val math_to_string : sb_math -> string
  (** Returns a string with sb_math converted into a S-expression  *)

val in_sbml : in_channel -> sb_model
  (** Returns an sb_model read from input stream *)
