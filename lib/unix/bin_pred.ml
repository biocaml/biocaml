type confusion_matrix = {
  tp : int ;
  tn : int ;
  fp : int ;
  fn : int ;
}
[@@deriving sexp]

type curve = (float * confusion_matrix) array
[@@deriving sexp]

let zero = { tp = 0 ; tn = 0 ; fn = 0 ; fp = 0 }

let update accu ~threshold ~score ~label =
  match Float.(threshold < score), label with
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

let no_positives { tp ; fn = _ ; fp ; tn = _ } =
  tp = 0 && fp = 0

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
    Array.sort ~compare:(Fn.flip Poly.compare) r ;
    r
  in
  let np = Array.count labels ~f:ident in
  let nn = Array.count labels ~f:(fun x -> not x) in
  let initial = { tp = 0 ; tn = nn ; fp = 0 ; fn = np } in
  let rec loop acc current_threshold current_matrix i =
    if i = n then
      List.rev ((current_threshold, current_matrix) :: acc)
    else
      let score, label = examples.(i) in
      let acc =
        if Float.(score < current_threshold) then
          (current_threshold, current_matrix) :: acc
        else
          acc
      in
      let new_matrix =
        if label then
          { current_matrix
            with tp = current_matrix.tp + 1 ;
                 fn = current_matrix.fn - 1 }
        else
          { current_matrix
            with fp = current_matrix.fp + 1 ;
                 tn = current_matrix.tn - 1 }
      in
      loop acc score new_matrix (i + 1)
  in
  loop [] Float.infinity initial 0
  |> Array.of_list

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

let average_precision ~precision ~recall =
  let n = Array.length precision in
  Array.init (n - 1) ~f:(fun i ->
      precision.(i + 1) *. (recall.(i + 1) -. recall.(i))
    )
  |> Array.sum (module Float) ~f:Fn.id

let recall_precision_curve ~scores ~labels =
  let matrices = performance_curve ~scores ~labels in
  let curve = Array.map matrices ~f:(fun (_,m) ->
      if no_positives m then 0., 1. else recall m, precision m
    ) in
  let recall, precision = Array.unzip curve in
  curve, average_precision ~recall ~precision

let%expect_test "performance curve 1" =
  let scores = [| 2.1 ; 1.2 ; 5.6 ; 0. |] in
  let labels = [| true ; false ; true ; false |] in
  let curve = performance_curve ~scores ~labels in
  print_endline (Sexp.to_string_hum (sexp_of_curve curve)) ;
  [%expect "
    ((INF ((tp 0) (tn 2) (fp 0) (fn 2))) (5.6 ((tp 1) (tn 2) (fp 0) (fn 1)))
     (2.1 ((tp 2) (tn 2) (fp 0) (fn 0))) (1.2 ((tp 2) (tn 1) (fp 1) (fn 0)))
     (0 ((tp 2) (tn 0) (fp 2) (fn 0))))"]

let%test "rp_curve perfect recognition" =
  let scores = [| 2.1 ; 1.2 ; 5.6 ; 0. |] in
  let labels = [| true ; false ; true ; false |] in
  let _, auc = recall_precision_curve ~scores ~labels in
  Float.(auc = 1.)

let%test "rp_curve against sklearn" =
  let scores = [|
    -0.20078869;  0.30423874;  0.20105976;  0.27523711;  0.42593404;
    -0.15043726; -0.08794601; -0.12733462;  0.22931596; -0.23913518;
    -0.06386267; -0.14958466; -0.04914839;  0.09898417;  0.0515638 ;
    -0.1142941 ;  0.16106135;  0.04871897; -0.08258102; -0.26105668;
    0.24693291; -0.18029058; -0.38384994;  0.26336904;  0.12585371;
    -0.03991278;  0.39424539;  0.42411536; -0.4790443 ; -0.30529061;
    -0.09281931;  0.01213433; -0.20204098;  0.40148935; -0.04536122;
    0.12179099;  0.06493837; -0.07007139;  0.0032915 ; -0.39635676;
    0.02619439;  0.20018683;  0.065023  ;  0.49589616; -0.28221895;
    0.31364573;  0.1906223 ;  0.11549516;  0.03145977;  0.22408591 |] in
  let labels = [|
    true; true; true; true; true; false; true; false; true; false; false; true; false; false; true; false; false; false; true; false; true; false;
    false; true; true; true; true; true; false; false; false; true; false; true; false; true; false; false; false; false; true; true; true; true;
    false; true; true; false; true; false |] in
  let curve, _ = recall_precision_curve ~scores ~labels in
  let recall, precision = Array.unzip curve in
  let ap = average_precision ~recall ~precision in
  Float.robustly_compare ap 0.8783170534965226 = 0
