open Biocaml_internal_pervasives

type confusion_matrix = {
  tp : int ;
  tn : int ;
  fp : int ;
  fn : int ;
}

let make ~pos ~neg = 
  let pos = Array.of_stream pos
  and neg = Array.of_stream neg in
  Array.sort (Fn.flip compare) pos ;
  Array.sort (Fn.flip compare) neg ;
  let sorted_elements = 
    Stream.merge
      ~cmp:(Fn.flip compare)
      (Array.to_stream pos |! Stream.map ~f:(fun x -> x, `pos))
      (Array.to_stream neg |! Stream.map ~f:(fun x -> x, `neg))
  and initial = {
    tp = 0 ;
    tn = Array.length neg ;
    fp = 0 ;
    fn = Array.length pos
  } 
  in
    Stream.append
      (Stream.singleton (Float.infinity, initial))
      (Stream.unfold
	 initial
	 (fun accu -> 
	   if Stream.is_empty sorted_elements then None
	   else match Stream.next sorted_elements with
	     | Some (x, `pos) -> 
	       let next = { accu with tp = accu.tp + 1 ; fn = accu.fn - 1 } in
	       Some ((x, next), next)
	     | Some (x, `neg) ->
	       let next = { accu with fp = accu.fp + 1 ; tn = accu.tn - 1 } in
	       Some ((x, next), next)
	     | None -> None))

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

let auc points = match Stream.next points with
    None -> 0.
  | Some p ->
    Stream.fold
      points
      ~f:(fun ((x1,y1), sum) ((x2,y2) as p) -> 
	(p, sum +. trapez_area x1 x2 y1 y2))
      ~init:(p, 0.)
    |! snd
