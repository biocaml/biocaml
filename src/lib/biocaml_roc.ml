open Batteries

type confusion_matrix = {
  tp : int ;
  tn : int ;
  fp : int ;
  fn : int ;
}

let make ~pos ~neg = 
  let pos = Array.of_enum pos
  and neg = Array.of_enum neg in
  Array.sort compare pos ;
  Array.sort compare neg ;
  let sorted_elements = 
    Enum.merge
      (fun x y -> x <= y)
      (Array.enum pos /@ (fun x -> x, `pos))
      (Array.enum neg /@ (fun x -> x, `neg))
  and initial = {
    tp = Array.length pos ;
    tn = 0 ;
    fp = Array.length neg ;
    fn = 0
  } 
  in
  initial, 
  Enum.unfold
    initial
    (fun accu -> 
      if Enum.is_empty sorted_elements then None
      else match Enum.get sorted_elements with
	| Some (x, `pos) -> 
	  let next = { accu with tp = accu.tp - 1 ; fn = accu.fn + 1 } in
	  Some ((x, next), next)
	| Some (x, `neg) ->
	  let next = { accu with fp = accu.fp - 1 ; tn = accu.tn + 1 } in
	  Some ((x, next), next)
	| None -> None)

let positive cm = cm.tp + cm.fn
let negative cm = cm.tn + cm.fp

let cardinal cm = cm.tp + cm.tn + cm.fp + cm.fn

let sensitivity cm = 
  float cm.tp /. float (cm.tp + cm.fn)

let false_positive_rate cm =
  float cm.fp /. float (cm.fp + cm.tn)

let accuracy cm =
  float (cm.tp + cm.tn) /. float (cardinal cm)

let specificity cm = 
  float cm.tn /. float (cm.fp + cm.tn)

let positive_predictive_value cm = 
  float cm.tp /. float (cm.tp + cm.fp)

let negative_predictive_value cm =
  float cm.tn /. float (cm.tn + cm.fn)

let false_discovery_rate cm =
  float cm.fp /. float (cm.fp + cm.tp)

let f1_score cm =
  2. *. float cm.tp /. float (2 * cm.tp + cm.fp + cm.fn)

let trapez_area x1 x2 y1 y2 = 0.5 *. (y1 +. y2) *. (x2 -. x1)

let auc points = match Enum.get points with
    None -> 0.
  | Some p ->
    Enum.fold
      (fun ((x1,y1), sum) ((x2,y2) as p) -> 
	(p, sum +. trapez_area x1 x2 y1 y2))
      (p, 0.)
      points
    |> snd

