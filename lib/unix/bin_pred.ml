open Core_kernel

type confusion_matrix = {
  tp : int ;
  tn : int ;
  fp : int ;
  fn : int ;
}

let zero = { tp = 0 ; tn = 0 ; fn = 0 ; fp = 0 }

let update accu ~threshold ~score ~label =
  match threshold < score, label with
  | true, true -> { accu with tp = accu.tp + 1 }
  | true, false -> { accu with fp = accu.fp + 1 }
  | false, true -> { accu with fn = accu.fn + 1 }
  | false, false -> { accu with tn = accu.tn + 1 }

let confusion_matrix ~scores ~labels ~threshold =
  if Array.length scores <> Array.length labels
  then
    invalid_argf
      "Bin_pred.confusion_matrix: scores and labels have different lengths (%d and %d)"
      (Array.length scores) (Array.length labels) () ;
  Array.fold2_exn scores labels ~init:zero ~f:(fun accu score label -> update accu ~threshold ~score ~label)

let positive cm = cm.tp + cm.fn
let negative cm = cm.tn + cm.fp

let cardinal cm = cm.tp + cm.tn + cm.fp + cm.fn

let sensitivity cm =
  float cm.tp /. float (cm.tp + cm.fn)

let recall = sensitivity

let false_positive_rate cm =
  float cm.fp /. float (cm.fp + cm.tn)

let accuracy cm =
  float (cm.tp + cm.tn) /. float (cardinal cm)

let specificity cm =
  float cm.tn /. float (cm.fp + cm.tn)

let positive_predictive_value cm =
  float cm.tp /. float (cm.tp + cm.fp)

let precision = positive_predictive_value

let negative_predictive_value cm =
  float cm.tn /. float (cm.tn + cm.fn)

let false_discovery_rate cm =
  float cm.fp /. float (cm.fp + cm.tp)

let f1_score cm =
  2. *. float cm.tp /. float (2 * cm.tp + cm.fp + cm.fn)

let performance_curve ~scores ~labels =
  let n = Array.length scores in
  if n <> Array.length labels
  then
    invalid_argf
      "Bin_pred.make_curve: scores and labels have different lengths (%d and %d)"
      n (Array.length labels) () ;
  let examples =
    let r = Array.map2_exn scores labels ~f:(fun x y -> x, y) in
    Array.sort ~cmp:(Fn.flip compare) r ;
    r
  in
  let np = Array.count labels ~f:ident in
  let nn = Array.count labels ~f:(fun x -> not x) in
  let initial = Float.infinity, { tp = 0 ; tn = nn ; fp = 0 ; fn = np } in
  let r = Array.create ~len:(n + 2) initial in
  for i = 0 to n - 1 do
    let score, label = examples.(i) in
    let m = snd r.(i) in
    let m' =
      if label then
	{ m with tp = m.tp + 1 ; fn = m.fn - 1 }
      else
	{ m with fp = m.fp + 1 ; tn = m.tn - 1 }
    in
    r.(i + 1) <- (score,m')
  done ;
  r.(n + 1) <- Float.neg_infinity, { tp = np ; tn = 0 ; fp = nn ; fn = 0 } ;
  r

let trapez_area x1 x2 y1 y2 = 0.5 *. (y1 +. y2) *. (x2 -. x1)

(* Assumes [points] is non empty (which is the case if it has been
   produced by [performance_curve]) and that points come with
   decreasing x-coordinates. *)
let auc points =
  let f ((x1,y1), sum) ((x2,y2) as p) = (p, sum +. trapez_area x1 x2 y1 y2) in
  Array.fold points ~f ~init:(points.(0), 0.) |> snd

let roc_curve ~scores ~labels =
  let matrices = performance_curve ~scores ~labels in
  let curve = Array.map matrices ~f:(fun (_,m) -> false_positive_rate m, sensitivity m) in
  let auc = auc curve in
  curve, auc

let rp_curve ~scores ~labels =
  let matrices = performance_curve ~scores ~labels in
  Array.map matrices ~f:(fun (_,m) -> recall m, precision m)
