open Batteries

type confusion_matrix = private {
  tp : int ;
  tn : int ;
  fp : int ;
  fn : int ;
}

val make : pos:float Enum.t -> neg:float Enum.t -> (float * confusion_matrix) Enum.t


val sensitivity : confusion_matrix -> float
val false_positive_rate : confusion_matrix -> float
val accuracy : confusion_matrix -> float
val specificity : confusion_matrix -> float
val positive_predictive_value : confusion_matrix -> float
val negative_predictive_value : confusion_matrix -> float
val false_discovery_rate : confusion_matrix -> float
val f1_score : confusion_matrix -> float
