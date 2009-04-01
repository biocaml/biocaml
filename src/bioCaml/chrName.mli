(** Chromosome names. A chromosome name, as defined by this module, consists of two parts. An optional prefix "chr", "Chr", or "CHR", followed by a suffix identifying the chromosome. The possible suffixes are:
    - "X" representing the maternal chromosome
    - "Y" representing the paternal chromosome
    - "M", "Mt", or "MtDNA" representing mitochondrial DNA
    - "N" where N is a positive number in either arabic or roman numeral form
    
    Some suffixes have ambiguous interpretations, e.g. "X" could be a Roman numeral or the maternal chromosome. Preference is given to the non-numeric interpretation. Thus "chrX" is treated as the maternal chromosome, not chromosome 10.
    Strings not in the above form are left unaltered by the functions of this module. 
    
    Functions for converting to an Arabic or Roman format are provided. The benefit is that these functions convert to a canonical form, and thus the resulting strings can be compared reliably. For example, [arabic "Chr4"] and [arabic "IV"] both return "4". The Arabic format is preferred over the Roman form because the latter is incomplete; it cannot represent chromosome number 10 because there would be an ambiguity with the maternal chromosome "chrX". *)

val arabic : string -> string
  (** [arabic s] returns the canonical Arabic representation of [s]. *)
  
val roman : string -> string
  (** [roman s] returns the canonical Roman representation of [s]. Raise [Failure] if [s] represents chromosome number 10, or any other chromosome that would cause an ambiguity. *)
