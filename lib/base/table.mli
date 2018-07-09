module Field : sig
  type 'a result = ('a, string) Result.t
  type 'a parser = string -> 'a result

  val int : int parser
  val positive_int : int parser
  val bounded_int : lo:int -> hi:int -> int parser
  val string_with_no_sep : string parser
  val parse : ?ctx:string -> 'a parser -> 'a parser
  val parse_all : ?ctx:string -> 'a parser -> string list -> 'a list result
end
