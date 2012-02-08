open Batteries

type confusion_matrix = private {
  tp : int ;
  tn : int ;
  fp : int ;
  fn : int ;
}

val make : pos:float Enum.t -> neg:float Enum.t -> confusion_matrix * (float * confusion_matrix) Enum.t
(** Given an enum [pos] (resp. [neg]) of scores from positive (resp. negative) instances, [make ~pos ~neg] 
    builds an enum of confusion matrices by setting an increasing acceptance threshold. More precisely, 
    [make ~pos ~neg] is a pair, whose first member is the confusion matrix obtained with an accept-all 
    threshold ([neg_infinity]); the second member enumerates the confusion matrices for all scores appearing
    in [pos] and [neg]. *)

val sensitivity : confusion_matrix -> float
val false_positive_rate : confusion_matrix -> float
val accuracy : confusion_matrix -> float
val specificity : confusion_matrix -> float
val positive_predictive_value : confusion_matrix -> float
val negative_predictive_value : confusion_matrix -> float
val false_discovery_rate : confusion_matrix -> float
val f1_score : confusion_matrix -> float

val auc : (float * float) Enum.t -> float
(** [auc e] computes the area above the X-axis and under the piecewise linear curve 
    passing through the points in [e]. Assumes that the points come with increasing 
    x-coordinate. *)
