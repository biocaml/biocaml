(** Numeric mathematics. *)


(** {6 Arithmetic Operations} *)

val log : ?base:float -> float -> float
  (** Logarithm. Default to natural logarithm if [base] not given. *)
  
val log10 : float -> float
  (** Base 10 logarithm. *)
  
val log2 : float -> float
  (** Base 2 logarithm. *)
  
val even : int -> bool
  (** True if given integer is even. *)
  
val odd : int -> bool
  (** True if given integer is odd. *)
  
val min : float array -> float
  (** Return minimum value in given array. Behavior undefined if array length = 0. *)
  
val max : float array -> float
  (** Return maximum value in given array. Behavior undefined if array length must = 0. *)
  
val range : float -> float -> float -> float array
  (** [range step first last] returns array \[|first; first +. step; ... |\], where last element will be less than or equal to [last]. If [first > last], [step] subtracted and last element must be greater than or equal to [last]. In either case, [step] must be positive. *)
  

(** {6 Statistical Operations} *)
  
val mean : float array -> float
  (** Mean. Behavior undefined if array length = 0. *)
  
val variance : float array -> float
  (** Variance. Behavior undefined if array length < 2. *)
  
val rms : float array -> float
  (** Root mean square. Behavior undefined if array length = 0. *)
  
val stdv : float array -> float
  (** Standard deviation. Behavior undefined if array length < 2. *)
  
val median : float array -> float
  (** Median. Behavior undefined if array length = 0. *)
  
val pseudomedian : float array -> float
  (** Pseudomedian is the median of all pairwise averages of values in given array (not including self-pairs). Behavior undefined if array length = 0. *)
  
val mad : float array -> float
  (** Median absolute deviation (MAD). Behavior undefined if array length = 0. *)
  
val quantile_normalization : float array array -> float array array
  (** Input matrix [m] should be arranged such that [m.(i).(j)] is the [i]th measurement in experiment [j]. Behavior undefined if [m] is not rectangular. *)
  
val histogram : ?cmp:('a -> 'a -> int) -> 'a array -> (('a * int) array)
  (** Return histogram of values using [cmp] (default = [Pervasives.compare]) for comparison. *)    

val prediction_values : int -> int -> int -> int -> (float * float * float * float)
  (** [prediction_values tp tn fp fn] takes 4 arguments: the number of true-positives [tp], true-negatives [tn], false-positives [fp], and false-negatives [fn]. It returns a quadruple of 4 measures of predictions accuracy: sensitivity, specificity, positive prediction accuracy, and negative prediction accuracy. *)

val pearson : float array -> float array -> float
  (** [pearson arr1 arr2] computes the Pearson product-moment correlation coefficient of two float arrays. See wikipedia for the formula. NB: everything is divided by n, not by n - 1. *)

(** {6 Matrix Operations} *)
  
val row : 'a array array -> int -> 'a array
  (** [row m i] returns the [i]th row of matrix [m]. By convention this is [m.(i)], but a copy is returned. Raise [Failure] if [m] does not contain at least [i+1] rows. *)
  
val column : 'a array array -> int -> 'a array
  (** [column m i] extracts the [i]th column of matrix [m]. Raise [Failure] if every row of [m] does not have at least [i+1] columns. See also {!row}. *)
  
val transpose : 'a array array -> 'a array array
  (** Transpose given matrix [m]. Okay if number of rows ({!length} [m]) is 0. If there are rows, they must not be empty; raise [Failure] if they are. Behavior undefined if [m] is not rectangular. *)
  

(** {6 More Specialized Operations} *)
  
val idxsort : ('a -> 'a -> int) -> 'a array -> int array
  (** [idxsort cmp a] is like {!Array.sort} but [a] is unaltered, and instead an array of the indices in sorted order is returned. E.g. [idxsort compare \[|'c'; 'd'; 'b'|\]] will return [\[|2; 0; 1|\]]. *)
  
val find_regions : ?max_gap:int -> ('a -> bool) -> 'a array -> (int * int) array
  (** [find_regions ~max_gap pred a] returns an array of [(first,last)] index pairs denoting boundaries (inclusive) of regions found in [a]. Each region is the longest contiguous sequence of values in [a] satisfying [pred]. A maximum of [max_gap] values within the region are allowed to fail [pred] but still get counted as if they had satisfied it. For example, [find_regions ~max_gap:1 (fun k -> k >= 3) \[|1; 3; 3; 2; 3; 1; 1; 3; 3|\]] will return [\[|(1,4); (7,8)|\]]. Default [max_gap = 0], raise [Failure] if set to negative value. *)

val find_min_window : ?init_direction:string -> 'a array -> (int -> int -> bool) -> int -> 'a array
  (** [find_min_window a pred i] finds the minimum sized window within [a] centered around index [i] that satisfies [pred]. Function [pred] is passed the window's start and end indices. Successively larger windows are created starting from \[i, i\] and the first one to satisfy [pred] is returned. An empty array is returned if the maximum window size, i.e. all of [a], is reached and [pred] still fails. Raise [Failure] if [i] is not a valid index for [a].
      
      The first window tried is \[i, i\], by default the second is \[i, i+1\], the third \[i-1, i+1\], the fourth \[i-1, i+2\], and so on. The optional [init_direction] must be either "fwd" or "rev". If "fwd", which is the default, the window size is initially increased in the forward direction. If "rev", the second window tried will be \[i-1, i\]. If the array's boundary is reached on either side, the size continues to be increased by incrementing on the opposing side. *)
  
