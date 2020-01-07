(*A module for parsing SBML level 2 version 4*)

exception Bad of string
let raise_bad msg = raise (Bad msg)

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

(*a wrapper type to deal with heterogeneous lists*)
type sb_L = LFunctionDefinition of sb_function_definition | LUnitDefinition of sb_unit_definition |
            LCompartment of sb_compartment | LSpecies of sb_species | LReaction of sb_reaction |
            LParameter of sb_parameter | LInitialAssignment of sb_initial_assignment | LRule of sb_rule |
            LEvent of sb_event | LEventAssignment of sb_event_assignment | LSpecieRef of sb_species_ref |
            LUnit of sb_unit

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

let ignore_input i =
  ignore (Xmlm.input i : Xmlm.signal)

module MathML = struct
  (* MathML prettyprinting *)
  let rec math_to_string math =
    let operator_to_string oper =
      match oper with
      | MPlus -> "+"
      | MMinus -> "-"
      | MTimes -> "*"
      | MPower -> "^"
      | MAbs -> "ABS"
      | MExp -> "EXP"
      | MFactorial -> "FACTORIAL"
      | MCeiling -> "CEILING"
      | MLt -> "<"
      | MGt -> ">"
      | MLeq -> "<="
      | MGeq -> ">="
      | MDelay -> "DELAY"
      | MFundef oper -> oper
      | _ -> raise_bad "can't convert unknown operator"
    in
    match math with
    | MApply (oper, exprlist) -> "(" ^ (operator_to_string oper) ^ " " ^ (String.concat ~sep:" " (List.map ~f:math_to_string exprlist)) ^ ")"
    | MLambda (bvarlist, lambda_expr) -> "(LAMBDA (" ^ (String.concat ~sep:" " bvarlist) ^ ") " ^ (math_to_string lambda_expr) ^ ")"
    | MPiecewise (piecelist, otherwise) -> "(PIECEWISE " ^ (String.concat ~sep:" " (List.map
                                                                                 ~f:(fun next -> let (var, varexpr) = next in "(" ^ (math_to_string varexpr) ^ " " ^ var ^ ")") piecelist))
                                           ^ " " ^ otherwise ^ ")"
    | MFloatNumber f -> (string_of_float f)
    | MIntNumber i -> (string_of_int i)
    | MIdentifier s -> s
    | MTime -> "<time>"
    | MExponent -> "e"
    | MNoMath -> "/no math/"
    | _ -> raise_bad "can't convert unknown math expr"

  (* MathML parsing *)

  let extract_string i depth errmsg =
    let rec skip_tags i depth =
      if depth > 0 then begin ignore_input i; skip_tags i (depth - 1) end else ()
    in
    skip_tags i depth;
    let result = match Xmlm.input i with
      | `Data dat -> dat
      | _ -> raise_bad errmsg
    in
    skip_tags i depth;
    result

  let unpack_string s =
    match s with
    | MIdentifier(str) -> str
    | _ -> raise_bad "not a packed string"

  let unpack_symbol_type attrs =
    let (_,sbmlUrl) = List.find_exn ~f:(fun next -> let ((_,tag), _) = next in String.equal tag "definitionURL") attrs in
    let splitUrl = String.split ~on:'/' sbmlUrl in
    List.nth_exn splitUrl (List.length splitUrl - 1)

  let parse_bvarlist i =
    let rec bvarlist_iter i bvarlist =
      match Xmlm.peek i with
      | `El_start ((_, "bvar"), _) -> bvarlist_iter i ((extract_string i 2 "malformed lambda expr in bvar") :: bvarlist)
      | `El_start ((_, _), _) -> bvarlist
      | _ -> raise_bad "malformed lambda expr in bvar list"
    in
    List.rev (bvarlist_iter i [])

  let rec parse_mathexpr i =
    let rec mathexpr_iter i formula =
      match Xmlm.input i with
      | `El_start ((_, "apply"), _) -> let operator = parse_operator i in
        let exprlist = parse_exprlist i in mathexpr_iter i (MApply (operator, exprlist))
      | `El_start ((_, "lambda"), _) -> let bvars = parse_bvarlist i in
        let lambda_expr = parse_mathexpr i in mathexpr_iter i (MLambda (bvars, lambda_expr))
      | `El_start ((_, "piecewise"), _) -> let pieces = parse_piecelist i in
        let otherwise = extract_string i 2 "malformed otherwise expr" in mathexpr_iter i (MPiecewise (pieces, otherwise))
      | `El_start ((_, "ci"), _) -> mathexpr_iter i (MIdentifier (unpack_string (parse_mathexpr i)))
      | `El_start ((_, "cn"), attrs) -> if (List.length attrs) = 1
        then match List.hd_exn attrs with
          | ((_, "type"), "integer") -> mathexpr_iter i (MIntNumber (int_of_string (unpack_string (parse_mathexpr i))))
          | ((_, "type"), "e-notation") ->
            mathexpr_iter i (MFloatNumber (float_of_string (unpack_string (parse_mathexpr i))))
          | ((_, _), _) -> raise_bad "malformed cn tag"
        else mathexpr_iter i (MFloatNumber (float_of_string (unpack_string (parse_mathexpr i))))
      | `El_start ((_, "sep"), _) -> mathexpr_iter i (MIdentifier ((unpack_string (formula)) ^ "e" ^ (unpack_string (parse_mathexpr i))))
      | `El_start ((_, "csymbol"), attrs) ->
        if String.equal (unpack_symbol_type attrs) "time" then
          begin ignore_input i; mathexpr_iter i (MTime) end
        else raise_bad "malformed csymbol expr"
      | `El_start ((_, "exponentiale"), _) -> mathexpr_iter i (MExponent)

      (* add more tokens *)

      | `El_start ((_, tag), _) -> print_endline tag; raise_bad "unknown math tag"
      | `Data dat -> MIdentifier (dat)
      | `El_end -> formula
      | `Dtd _ -> assert false
    in
    mathexpr_iter i MNoMath
  and
    parse_operator i =
    let oper = match Xmlm.input i with
      | `El_start ((_, "plus"), _) -> MPlus
      | `El_start ((_, "minus"), _) -> MMinus
      | `El_start ((_, "times"), _) -> MTimes
      | `El_start ((_, "power"), _) -> MPower
      | `El_start ((_, "abs"), _) -> MAbs
      | `El_start ((_, "exp"), _) -> MExp
      | `El_start ((_, "factorial"), _) -> MFactorial
      | `El_start ((_, "ceiling"), _) -> MCeiling
      | `El_start ((_, "lt"), _) -> MLt
      | `El_start ((_, "gt"), _) -> MGt
      | `El_start ((_, "leq"), _) -> MLeq
      | `El_start ((_, "geq"), _) -> MGeq

      (* add more operators *)

      | `El_start ((_, "csymbol"), attrs) ->
        if String.equal (unpack_symbol_type attrs) "delay" then
          begin ignore_input i; MDelay end
        else raise_bad "malformed csymbol expr"
      (* assume a user-defined function in functionDefinition*)
      | `El_start ((_, "ci"), _) -> MFundef (unpack_string (parse_mathexpr i))
      | _ -> raise_bad "malformed apply expr"
    in ignore_input i; oper
  and
    parse_exprlist i =
    let rec exprlist_iter i exprlist =
      match Xmlm.peek i with
      | `El_start ((_, _), _) -> exprlist_iter i ((parse_mathexpr i) :: exprlist)
      | `El_end -> exprlist
      | _ -> raise_bad "malformed mathml in apply"
    in
    List.rev (exprlist_iter i [])
  and
    parse_piecelist i =
    let rec piecelist_iter i piecelist =
      match Xmlm.peek i with
      | `El_start ((_, "piece"), _) -> ignore_input i;
        let piece_var = extract_string i 1 "malformed piece expr" in
        let piece_expr = parse_mathexpr i in
        ignore_input i;
        piecelist_iter i ((piece_var, piece_expr) :: piecelist)
      | `El_start ((_, "otherwise"), _) -> piecelist
      | _ -> raise_bad "malformed piecewise expr"
    in
    List.rev (piecelist_iter i [])

  let parse_math _ i =
    let sbm = parse_mathexpr i in
    ignore_input i; (*math tag end*)
    sbm
end

let parse_math = MathML.parse_math
let math_to_string = MathML.math_to_string

module SBMLParser = struct

  (*abstract stuff for lists and attributes*)
  let store_attrs attrs =
    let parse_hash = Caml.Hashtbl.create 10 in
    let store_attr attr =
      match attr with
      | ((_, nam), value) -> Caml.Hashtbl.add parse_hash nam value
    in List.iter ~f:store_attr attrs; parse_hash

  let parse_list i assoclist =
    let rec iter_list i templist =
      match Xmlm.input i with
      | `El_start ((_, tagname), attrs) -> iter_list i ((try ((Caml.List.assoc tagname assoclist) attrs i) with Caml.Not_found -> raise_bad tagname) :: templist)
      | `El_end -> templist
      | `Data _ -> iter_list i templist
      | `Dtd _ -> assert false
    in
    iter_list i []

  let parse_record i list_dict record_dict =
    let list_hash = Caml.Hashtbl.create 10 in
    let record_hash = Caml.Hashtbl.create 10 in
    let rec iter_record i =
      match Xmlm.input i with
      | `El_start ((_, tagname), attrs) -> (if (String.compare (String.sub tagname ~pos:0 ~len:4) "list")=0
                                            then (try (Caml.Hashtbl.add list_hash tagname (parse_list i (Caml.List.assoc tagname list_dict))) with Caml.Not_found -> raise_bad tagname)
                                            else (try (Caml.Hashtbl.add record_hash tagname ((Caml.List.assoc tagname record_dict) attrs i)) with Caml.Not_found -> raise_bad tagname));
        iter_record i
      | `El_end -> ()
      | `Data _ -> iter_record i
      | `Dtd _ -> assert false
    in iter_record i; (list_hash, record_hash)

  (*leaf record parsing, 'ignore_input i' is for skipping tag's end *)

  let parse_unit attrs i =
    ignore_input i; let parse_hash = store_attrs attrs in
    LUnit ({
        unit_kind = (Caml.Hashtbl.find parse_hash "kind");
        unit_exponent = (try (int_of_string (Caml.Hashtbl.find parse_hash "exponent")) with Caml.Not_found -> 1);
        unit_scale = (try int_of_string (Caml.Hashtbl.find parse_hash "scale") with Caml.Not_found -> 0);
        unit_multiplier = (try (float_of_string (Caml.Hashtbl.find parse_hash "multiplier")) with Caml.Not_found -> 1.0)
      })

  let parse_compartment attrs i =
    ignore_input i; let parse_hash = store_attrs attrs in
    LCompartment ({
        compart_id = (Caml.Hashtbl.find parse_hash "id");
        compart_name = (try (Caml.Hashtbl.find parse_hash "name") with Caml.Not_found -> "");
        compart_size = (try (float_of_string (Caml.Hashtbl.find parse_hash "size")) with Caml.Not_found -> 0.0);
        compart_spatialDimensions = (try (int_of_string  (Caml.Hashtbl.find parse_hash "spatialDimensions")) with Caml.Not_found -> 3);
        compart_units = (try (Caml.Hashtbl.find parse_hash "units") with Caml.Not_found -> "");
        compart_outside = (try (Caml.Hashtbl.find parse_hash "outside") with Caml.Not_found -> "");
        compart_constant = (try (bool_of_string (Caml.Hashtbl.find parse_hash "constant")) with Caml.Not_found -> true);
      })

  let parse_species attrs i =
    ignore_input i; let parse_hash = store_attrs attrs in
    LSpecies ({
        species_id = (Caml.Hashtbl.find parse_hash "id");
        species_name = (try (Caml.Hashtbl.find parse_hash "name") with Caml.Not_found -> "");
        species_type = (try (Caml.Hashtbl.find parse_hash "speciesType") with Caml.Not_found -> "");
        species_compartment = (Caml.Hashtbl.find parse_hash "compartment");
        species_initialAmount = (try (float_of_string (Caml.Hashtbl.find parse_hash "initialAmount")) with Caml.Not_found -> 0.0);
        species_initialConcentration = (try (float_of_string (Caml.Hashtbl.find parse_hash "initialConcentration")) with Caml.Not_found -> 0.0);
        species_substanceUnits = (try (Caml.Hashtbl.find parse_hash "substanceUnits") with Caml.Not_found -> "");
        species_hasOnlySubstanceUnits = (try (bool_of_string (Caml.Hashtbl.find parse_hash "hasOnlySubstanceUnits")) with Caml.Not_found -> false);
        species_boundaryCondition = (try (bool_of_string (Caml.Hashtbl.find parse_hash "boundaryCondition")) with Caml.Not_found -> false);
        species_constant = (try (bool_of_string (Caml.Hashtbl.find parse_hash "constant")) with Caml.Not_found -> false);
      })

  let parse_spreference attrs i =
    ignore_input i; let parse_hash = store_attrs attrs in
    LSpecieRef ({
        specref_species = (Caml.Hashtbl.find parse_hash "species");
        specref_id = (try (Caml.Hashtbl.find parse_hash "id") with Caml.Not_found -> "");
        specref_name = (try (Caml.Hashtbl.find parse_hash "name") with Caml.Not_found -> "");
        specref_stoichiometry = (try (int_of_string (Caml.Hashtbl.find parse_hash "stoichiometry")) with Caml.Not_found -> 1)
      })

  let parse_parameter attrs i =
    ignore_input i; let parse_hash = store_attrs attrs in
    LParameter ({
        param_id = (try (Caml.Hashtbl.find parse_hash "id") with Caml.Not_found -> raise_bad "no id for parameter") ;
        param_name = (try (Caml.Hashtbl.find parse_hash "name") with Caml.Not_found -> "");
        param_value = (try (float_of_string (Caml.Hashtbl.find parse_hash "value")) with Caml.Not_found -> 0.0);
        param_units = (try (Caml.Hashtbl.find parse_hash "units") with Caml.Not_found -> "");
        param_constant = (try (bool_of_string (Caml.Hashtbl.find parse_hash "constant")) with Caml.Not_found -> true);
      })

  (*container record parsing*)

  let parse_fundef attrs i =
    let parse_hash = store_attrs attrs in
    let (_, record_hash) = parse_record i [] [("math",parse_math)] in
    LFunctionDefinition ({
        fundef_id = (Caml.Hashtbl.find parse_hash "id");
        fundef_name = (try (Caml.Hashtbl.find parse_hash "name") with Caml.Not_found -> "");
        fundef_math = (try (Caml.Hashtbl.find record_hash "math") with Caml.Not_found -> MNoMath);
      })

  let parse_unitdef attrs i =
    let parse_hash = store_attrs attrs in
    let (_, record_hash) = parse_record i [("listOfUnits",[("unit",parse_unit)])] [] in
    LUnitDefinition ({
        unitdef_id = (Caml.Hashtbl.find parse_hash "id");
        unitdef_name = (try (Caml.Hashtbl.find parse_hash "name") with Caml.Not_found -> "");
        unitdef_unitlist = (try (List.rev_map ~f:(function LUnit(t) -> t | _ -> raise_bad "unit")
            (Caml.Hashtbl.find record_hash "listOfUnits")) with Caml.Not_found -> []);
      })

  let parse_iassignment attrs i =
    let parse_hash = store_attrs attrs in
    let _, record_hash = parse_record i [] [ "math", parse_math ] in
    LInitialAssignment ({
        ia_symbol = (Caml.Hashtbl.find parse_hash "symbol");
        ia_math = (try (Caml.Hashtbl.find record_hash "math") with Caml.Not_found -> MNoMath);
      })

  let parse_generic_rule attrs i =
    let parse_hash = store_attrs attrs in
    let _, record_hash = parse_record i [] [ "math", parse_math ] in
    {
      gr_variable = (Caml.Hashtbl.find parse_hash "variable");
      gr_math = (try (Caml.Hashtbl.find record_hash "math") with Caml.Not_found -> MNoMath);
    }
  let parse_algebraic_rule _ i =
    let _, record_hash = parse_record i [] [ "math", parse_math ] in
    LRule (AlgebraicRule ({
        ar_math = (try (Caml.Hashtbl.find record_hash "math") with Caml.Not_found -> MNoMath);
      }))
  let parse_assignment_rule attrs i = LRule (AssignmentRule (parse_generic_rule attrs i))
  let parse_rate_rule attrs i = LRule (RateRule (parse_generic_rule attrs i))

  let parse_kineticlaw _ i =
    let (list_hash,record_hash) = (parse_record i [("listOfParameters",[("parameter",parse_parameter)])]
                                     [("math",parse_math)]) in
    {
      klaw_parameters = (try (List.rev_map ~f:(function LParameter(t) -> t | _ -> raise_bad "parameter")
          (Caml.Hashtbl.find list_hash "listOfParameters")) with Caml.Not_found -> []);
      klaw_math = (try (Caml.Hashtbl.find record_hash "math") with Caml.Not_found -> MNoMath);
    }

  let parse_reaction attrs i =
    let parse_hash = store_attrs attrs in
    let (list_hash,record_hash) = (parse_record i [("listOfReactants",[("speciesReference",parse_spreference)]);
                                                   ("listOfProducts",[("speciesReference",parse_spreference)])]
                                     [("kineticLaw",parse_kineticlaw)]) in
    LReaction ({
        react_id = (Caml.Hashtbl.find parse_hash "id");
        react_name = (try (Caml.Hashtbl.find parse_hash "name") with Caml.Not_found -> "");
        react_boundaryCondition = (try (bool_of_string (Caml.Hashtbl.find parse_hash "reversible")) with Caml.Not_found -> true);
        react_fast = (try (bool_of_string (Caml.Hashtbl.find parse_hash "fast")) with Caml.Not_found -> false);
        react_reactants = (try (List.rev_map ~f:(function LSpecieRef(t) -> t | _ -> raise_bad "malformed specieReference")
            (Caml.Hashtbl.find list_hash "listOfReactants")) with Caml.Not_found -> []);
        react_products = (try (List.rev_map ~f:(function LSpecieRef(t) -> t | _ -> raise_bad "malformed specieReference")
            (Caml.Hashtbl.find list_hash "listOfProducts")) with Caml.Not_found -> []);
        react_kineticLaw = (try (Caml.Hashtbl.find record_hash "kineticLaw")
                            with Caml.Not_found -> {klaw_parameters = []; klaw_math = MNoMath});
      })

  let parse_eassignment attrs i =
    let parse_hash = store_attrs attrs in
    let _, record_hash = parse_record i [] [ "math", parse_math ] in
    LEventAssignment ({
        ea_variable = (Caml.Hashtbl.find parse_hash "variable");
        ea_math = (try (Caml.Hashtbl.find record_hash "math") with Caml.Not_found -> MNoMath);
      })

  let parse_math_container _ i =
    let _, record_hash = parse_record i [] [ "math", parse_math ] in
    {
      math = (try (Caml.Hashtbl.find record_hash "math") with Caml.Not_found -> MNoMath);
    }

  let parse_event attrs i =
    let parse_hash = store_attrs attrs in
    let (list_hash,record_hash) = (parse_record i [("listOfEventAssignments",[("eventAssignment",parse_eassignment)])]
                                     [("trigger",parse_math_container);
                                      ("delay",parse_math_container)]) in
    LEvent ({
        event_id = (try (Caml.Hashtbl.find parse_hash "id") with Caml.Not_found -> "");
        event_name = (try (Caml.Hashtbl.find parse_hash "name") with Caml.Not_found -> "");
        event_useValuesFromTriggerTime = (try (bool_of_string (Caml.Hashtbl.find parse_hash "useValuesFromTriggerTime")) with Caml.Not_found -> true);
        event_trigger = (try Trigger (Caml.Hashtbl.find record_hash "trigger") with Caml.Not_found -> raise_bad "trigger not found in event");
        event_delay = (try Delay (Caml.Hashtbl.find record_hash "delay") with Caml.Not_found -> Delay ({math = MNoMath}));
        event_assignments = (try (List.rev_map ~f:(function LEventAssignment(t) -> t | _ -> raise_bad "malformed eventAssignment")
            (Caml.Hashtbl.find list_hash "listOfEventAssignments")) with Caml.Not_found -> []);
      })

  let parse_model attrs i =
    let parse_hash = store_attrs attrs in
    let list_hash, _ = parse_record i [("listOfFunctionDefinitions",[("functionDefinition",parse_fundef)]);
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
        [] in
    {
      sbm_id = (try (Caml.Hashtbl.find parse_hash "id") with Caml.Not_found -> "");
      sbm_name = (try (Caml.Hashtbl.find parse_hash "name") with Caml.Not_found -> "");
      sbm_functionDefinitions = (try (List.rev_map ~f:(function LFunctionDefinition(t) -> t | _ -> raise_bad "malformed functionDefinition")
          (Caml.Hashtbl.find list_hash "listOfFunctionDefinitions")) with Caml.Not_found -> []);
      sbm_unitDefinitions = (try (List.rev_map ~f:(function LUnitDefinition(t) -> t | _ -> raise_bad "malformed unitDefinition")
          (Caml.Hashtbl.find list_hash "listOfUnitDefinitions")) with Caml.Not_found -> []);
      sbm_compartments = (try (List.rev_map ~f:(function LCompartment(t) -> t | _ -> raise_bad "malformed compartment")
          (Caml.Hashtbl.find list_hash "listOfCompartments")) with Caml.Not_found -> []);
      sbm_species = (try (List.rev_map ~f:(function LSpecies(t) -> t | _ -> raise_bad "malformed species")
          (Caml.Hashtbl.find list_hash "listOfSpecies")) with Caml.Not_found -> []);
      sbm_reactions = (try (List.rev_map ~f:(function LReaction(t) -> t | _ -> raise_bad "malformed reaction")
          (Caml.Hashtbl.find list_hash "listOfReactions")) with Caml.Not_found -> []);
      sbm_parameters = (try (List.rev_map ~f:(function LParameter(t) -> t | _ -> raise_bad "malformed parameter")
          (Caml.Hashtbl.find list_hash "listOfParameters")) with Caml.Not_found -> []);
      sbm_initialAssignments = (try (List.rev_map ~f:(function LInitialAssignment(t) -> t | _ -> raise_bad "malformed initialAssignment")
          (Caml.Hashtbl.find list_hash "listOfInitialAssignments")) with Caml.Not_found -> []);
      sbm_rules = (try (List.rev_map ~f:(function LRule(t) -> t | _ -> raise_bad "malformed rule")
          (Caml.Hashtbl.find list_hash "listOfRules")) with Caml.Not_found -> []);
      sbm_events = (try (List.rev_map ~f:(function LEvent(t) -> t | _ -> raise_bad "malformed event")
          (Caml.Hashtbl.find list_hash "listOfEvents")) with Caml.Not_found -> []);
    }

  (*reader function*)
  let in_sbml ichan =
    let i = (Xmlm.make_input ~strip:true (`Channel ichan)) in
    ignore_input i; (* `Dtd *)
    ignore_input i; (* smbl tag start *)
    let model =
      match Xmlm.input i with
      | `El_start ((_, "model"), attrs) -> parse_model attrs i
      | _ -> raise_bad "malformed sbml" in
    ignore_input i; (* smbl tag end *)
    if not (Xmlm.eoi i) then raise_bad "sbml too long";
    model

end

let in_sbml = SBMLParser.in_sbml
