(** Chromosome names. A chromosome name is one of the forms:
    - "chrN" where N is a positive number in either arabic or roman numeral form
    - "chrX" representing the maternal chromosome
    - "chrY" representing the paternal chromosome
    - "chrM", "chrMt", or "chrMtDNA" representing mitochondrial DNA
    
    The "chr" prefix can also be "Chr" or "CHR", or omitted completely. Any strings not in one of these forms will be unaltered by the functions of this module. Some suffixes, such as "X", have ambiguous interpretations: as a Roman numeral and one of the other above defined forms. The ambiguity is resolved by preferring the latter.
    
    Functions for converting to an Arabic or Roman format are provided. The benefit is that these functions convert to a canonical form, and thus the resulting strings can be compared reliably. For example, [arabic "Chr4"] and [arabic "IV"] both return "4". The Arabic format is preferred over the Roman form because the latter is incomplete; it cannot represent chromosome number 10 because there would be an ambiguity with "chrX". *)

val arabic : string -> string
  (** [arabic s] returns the canonical Arabic representation of [s]. *)
  
val roman : string -> string
  (** [roman s] returns the canonical Roman representation of [s]. For example, I. Raise [Failure] if [s] represents chromosome number 10, or any other chromosome that would cause an ambiguity. *)
