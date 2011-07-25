(* This file is part of guizmin.

    guizmin is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    guizmin is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with guizmin.  If not, see <http://www.gnu.org/licenses/>.
*)
open Batteries

module Location : sig
  type t = private {
    chr : string ;
    st : int ;
    ed : int
  }

  val make : string -> int -> int -> t

  val to_string : t -> string 
    (** String representation of a location, as <chr>:<start>-<end> *)
  
  val length : t -> int
    
  val of_string : string -> t
    (** Parses a string representation of a genomic location, of the
	form <string>:<int>-<int>, like chr1:23-45. Reciprocal to the
	function {!to_string}.  *)

  val included_in : t -> t -> bool

  val intersection : t -> t -> bool

  val inter : t -> t -> t
    
  val dist : t -> t -> int
    (** Both locations should be on the same chromosome, throws
	[Invalid_argument "Ucsc.Location.dist"] otherwise *)

  val position : from:t -> t -> int
  val compare : t -> t -> int
end

module Annotation : sig
  type 'a t 

  (** 3 Constructors *)
  val make : int -> (Location.t * 'a) Enum.t -> 'a t
  val of_array : (Location.t * 'a) array -> 'a t

  (** 3 Traversal *)
  val fold : (Location.t -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val fold_in : (Location.t -> 'a -> 'c -> 'c) -> 'a t -> Location.t -> 'c -> 'c
  val map  : (Location.t -> 'a -> 'b) -> 'a t -> 'b t

  (** 3 Visitors *)

  (** [nbregions_in map loc] is the number of locations 
      in [map] that intersect [loc]. *)
  val nbregions_in : 'a t -> Location.t -> int

  val regions_in : 'a t -> Location.t -> 'a array

  (** number of locations in the map *)
  val cardinal : 'a t -> int

  (** [closest_region loc map] raises [Not_found] if 
      there is no region in [map] on the same chromosome
      than [loc] *)
  val closest_region : Location.t -> 'a t -> Location.t * 'a * int

  (** same as [closest_region] but retrieves more locations *)
  val k_closest_regions : int -> Location.t -> 'a t -> (Location.t * 'a * int) array

  val intersects : Location.t -> 'a t -> bool
  val length : 'a t -> int

  (** 3 Partitions *)
  type 'a partition
  val of_partition   : 'a partition -> 'a t

  module Partition : sig
    val length : 'a partition -> int
    val fold : (Location.t -> 'a list -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val make : (Location.t -> 'a list -> 'b) -> 'a t -> 'b partition
    val filter : (Location.t -> 'a -> bool) -> 'a partition -> 'a partition
    val eval : na:'a -> chr:string -> pos:int -> map:'a partition -> 'a
  end

  (** 3 Conversion *)
  val to_array : (Location.t -> 'a -> 'b) -> 'a t -> 'b array
  val enum : 'a t -> (Location.t * 'a) Enum.t

  (** 3 Set operations *)
  val union : 'a t -> 'a t -> 'a t
  val union_partition : 'a t -> 'c t -> ('a list * 'c list) partition
  val intersection_partition : 'a t -> 'c t -> ('a list * 'c list) partition

  (** [mask map1 map2] is equal to [map1] minus the zones that are also in 
      [map2]. In the process, [map1] is partitioned, which explains why the 
      annotations are transformed into lists of annotations *)
  val mask : 'a t -> 'c t -> 'a list partition

  (** like [mask] except that as we know [map1] is a partition, we do not
      need to produce lists of annotations *)
  val mask_partition : 'a partition -> 'c t -> 'a partition

  (** 3 Utilities *)

  val leftjoin :
    ?up:int -> ?down:int ->
    ('a -> Location.t) ->
    ('b -> Location.t) ->
    'a array -> 'b array ->
    ('a * 'b array) array
end
