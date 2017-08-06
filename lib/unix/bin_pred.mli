(** Performance measurement of binary classifiers.

    This module provides functions to compute various performance
    measurements of a binary classifier's prediction. Typically, binary
    classifiers output both a label and a score indicating a
    confidence level. A ROC curve represents the variation of
    sensitivity and specificity of the classifier as a function of a
    score threshold.
 *)

open Core_kernel

type confusion_matrix = private {
  tp : int ;
  tn : int ;
  fp : int ;
  fn : int ;
}

val confusion_matrix : scores:float array -> labels:bool array -> threshold:float -> confusion_matrix
(** [confusion_matrix ~scores ~labels ~threshold] computes a confusion
    matrix from the classifier scores and example labels, based on a
    threshold. It assumes that example [i] has score [scores.(i)] and
    label [labels.(i)], that [scores] and [labels] have the same
    length and that a higher score means increased probability of a
    [true] label. *)

val positive : confusion_matrix -> int

val negative : confusion_matrix -> int

val cardinal : confusion_matrix -> int

val sensitivity : confusion_matrix -> float

val recall : confusion_matrix -> float
(** same as [sensitivity] *)

val false_positive_rate : confusion_matrix -> float

val accuracy : confusion_matrix -> float

val specificity : confusion_matrix -> float

val positive_predictive_value : confusion_matrix -> float

val precision : confusion_matrix -> float
(** same as [positive_predictive_value] *)

val negative_predictive_value : confusion_matrix -> float

val false_discovery_rate : confusion_matrix -> float

val f1_score : confusion_matrix -> float


val performance_curve : scores:float array -> labels:bool array -> (float * confusion_matrix) array
(** [performance_curve ~scores ~labels] returns the series of
    confusion matrices obtained by varying the threshold from
    [infinity] to [neg_infinity]. Each confusion matrix comes with the
    corresponding threshold. *)

val roc_curve : scores:float array -> labels:bool array -> (float * float) array * float
(** [roc_curve ~scores ~labels] returns the
    {{:http://en.wikipedia.org/wiki/Receiver_operating_characteristic}ROC}
    curve of the prediction, and the associated Area Under Curve (AUC) *)

val rp_curve : scores:float array -> labels:bool array -> (float * float) array
