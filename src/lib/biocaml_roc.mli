open Batteries

type confusion_matrix = private {
  tp : int ;
  tn : int ;
  fp : int ;
  fn : int ;
}

val make : pos:float Enum.t -> neg:float Enum.t -> (float * confusion_matrix) Enum.t
(** Given an enum [pos] (resp. [neg]) of scores from positive (resp. negative) instances, [make ~pos ~neg] 
    builds an enum of confusion matrices by setting an decreasing acceptance threshold. The result has at 
    least one first value, which is the confusion matrix for an [infinity] threshold. The subsequent
    thresholds are the values in [pos] and [neg]. *)

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
