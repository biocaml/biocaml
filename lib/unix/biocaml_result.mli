(** Extension of Core's Result. Internal use only. *)

include module type of Result

module List : sig
  val mapi : 'a list -> f:(int -> 'a -> ('b, 'e) t) -> ('b list, 'e) t
  val map : 'a list -> f:('a -> ('b, 'err) t) -> ('b list, 'err) t
end
