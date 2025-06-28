type confusion_matrix =
  { tp : int
  ; tn : int
  ; fp : int
  ; fn : int
  }
[@@deriving sexp]

type curve = (float * confusion_matrix) array [@@deriving sexp]

let zero = { tp = 0; tn = 0; fn = 0; fp = 0 }

let update accu ~threshold ~score ~label =
  match Float.(threshold < score), label with
  | true, true -> { accu with tp = accu.tp + 1 }
  | true, false -> { accu with fp = accu.fp + 1 }
  | false, true -> { accu with fn = accu.fn + 1 }
  | false, false -> { accu with tn = accu.tn + 1 }
;;

let confusion_matrix ~scores ~labels ~threshold =
  if Array.length scores <> Array.length labels
  then
    invalid_argf
      "Bin_pred.confusion_matrix: scores and labels have different lengths (%d and %d)"
      (Array.length scores)
      (Array.length labels)
      ();
  Array.fold2_exn scores labels ~init:zero ~f:(fun accu score label ->
    update accu ~threshold ~score ~label)
;;

let positive cm = cm.tp + cm.fn
let negative cm = cm.tn + cm.fp
let cardinal cm = cm.tp + cm.tn + cm.fp + cm.fn
let no_positives { tp; fn = _; fp; tn = _ } = tp = 0 && fp = 0
let sensitivity cm = float cm.tp /. float (cm.tp + cm.fn)
let recall = sensitivity
let false_positive_rate cm = float cm.fp /. float (cm.fp + cm.tn)
let accuracy cm = float (cm.tp + cm.tn) /. float (cardinal cm)
let specificity cm = float cm.tn /. float (cm.fp + cm.tn)
let positive_predictive_value cm = float cm.tp /. float (cm.tp + cm.fp)
let precision = positive_predictive_value
let negative_predictive_value cm = float cm.tn /. float (cm.tn + cm.fn)
let false_discovery_rate cm = float cm.fp /. float (cm.fp + cm.tp)
let f1_score cm = 2. *. float cm.tp /. float ((2 * cm.tp) + cm.fp + cm.fn)

let performance_curve ~scores ~labels =
  let n = Array.length scores in
  if n <> Array.length labels
  then
    invalid_argf
      "Bin_pred.make_curve: scores and labels have different lengths (%d and %d)"
      n
      (Array.length labels)
      ();
  let examples =
    let r = Array.map2_exn scores labels ~f:(fun x y -> x, y) in
    Array.sort ~compare:(Fn.flip Poly.compare) r;
    r
  in
  let np = Array.count labels ~f:Fun.id in
  let nn = Array.count labels ~f:(fun x -> not x) in
  let initial = { tp = 0; tn = nn; fp = 0; fn = np } in
  let rec loop acc current_threshold current_matrix i =
    if i = n
    then List.rev ((current_threshold, current_matrix) :: acc)
    else (
      let score, label = examples.(i) in
      let acc =
        if Float.(score < current_threshold)
        then (current_threshold, current_matrix) :: acc
        else acc
      in
      let new_matrix =
        if label
        then
          { current_matrix with tp = current_matrix.tp + 1; fn = current_matrix.fn - 1 }
        else
          { current_matrix with fp = current_matrix.fp + 1; tn = current_matrix.tn - 1 }
      in
      loop acc score new_matrix (i + 1))
  in
  loop [] Float.infinity initial 0 |> Array.of_list
;;

let trapez_area x1 x2 y1 y2 = 0.5 *. (y1 +. y2) *. (x2 -. x1)

(* Assumes [points] is non empty (which is the case if it has been
   produced by [performance_curve]) and that points come with
   decreasing x-coordinates. *)
let auc points =
  let f ((x1, y1), sum) ((x2, y2) as p) = p, sum +. trapez_area x1 x2 y1 y2 in
  Array.fold points ~f ~init:(points.(0), 0.) |> snd
;;

let roc_curve ~scores ~labels =
  let matrices = performance_curve ~scores ~labels in
  let curve =
    Array.map matrices ~f:(fun (_, m) -> false_positive_rate m, sensitivity m)
  in
  let auc = auc curve in
  curve, auc
;;

let average_precision ~precision ~recall =
  let n = Array.length precision in
  Array.init (n - 1) ~f:(fun i -> precision.(i + 1) *. (recall.(i + 1) -. recall.(i)))
  |> Array.sum (module Float) ~f:Fn.id
;;

let recall_precision_curve ~scores ~labels =
  let matrices = performance_curve ~scores ~labels in
  let curve =
    Array.map matrices ~f:(fun (_, m) ->
      if no_positives m then 0., 1. else recall m, precision m)
  in
  let recall, precision = Array.unzip curve in
  curve, average_precision ~recall ~precision
;;

let%expect_test "performance curve 1" =
  let scores = [| 2.1; 1.2; 5.6; 0. |] in
  let labels = [| true; false; true; false |] in
  let curve = performance_curve ~scores ~labels in
  print_endline (Sexp.to_string_hum (sexp_of_curve curve));
  [%expect
    "\n\
    \    ((INF ((tp 0) (tn 2) (fp 0) (fn 2))) (5.6 ((tp 1) (tn 2) (fp 0) (fn 1)))\n\
    \     (2.1 ((tp 2) (tn 2) (fp 0) (fn 0))) (1.2 ((tp 2) (tn 1) (fp 1) (fn 0)))\n\
    \     (0 ((tp 2) (tn 0) (fp 2) (fn 0))))"]
;;

let%test "rp_curve perfect recognition" =
  let scores = [| 2.1; 1.2; 5.6; 0. |] in
  let labels = [| true; false; true; false |] in
  let _, auc = recall_precision_curve ~scores ~labels in
  Float.(auc = 1.)
;;

let%test "rp_curve against sklearn" =
  let scores =
    [| -0.20078869
     ; 0.30423874
     ; 0.20105976
     ; 0.27523711
     ; 0.42593404
     ; -0.15043726
     ; -0.08794601
     ; -0.12733462
     ; 0.22931596
     ; -0.23913518
     ; -0.06386267
     ; -0.14958466
     ; -0.04914839
     ; 0.09898417
     ; 0.0515638
     ; -0.1142941
     ; 0.16106135
     ; 0.04871897
     ; -0.08258102
     ; -0.26105668
     ; 0.24693291
     ; -0.18029058
     ; -0.38384994
     ; 0.26336904
     ; 0.12585371
     ; -0.03991278
     ; 0.39424539
     ; 0.42411536
     ; -0.4790443
     ; -0.30529061
     ; -0.09281931
     ; 0.01213433
     ; -0.20204098
     ; 0.40148935
     ; -0.04536122
     ; 0.12179099
     ; 0.06493837
     ; -0.07007139
     ; 0.0032915
     ; -0.39635676
     ; 0.02619439
     ; 0.20018683
     ; 0.065023
     ; 0.49589616
     ; -0.28221895
     ; 0.31364573
     ; 0.1906223
     ; 0.11549516
     ; 0.03145977
     ; 0.22408591
    |]
  in
  let labels =
    [| true
     ; true
     ; true
     ; true
     ; true
     ; false
     ; true
     ; false
     ; true
     ; false
     ; false
     ; true
     ; false
     ; false
     ; true
     ; false
     ; false
     ; false
     ; true
     ; false
     ; true
     ; false
     ; false
     ; true
     ; true
     ; true
     ; true
     ; true
     ; false
     ; false
     ; false
     ; true
     ; false
     ; true
     ; false
     ; true
     ; false
     ; false
     ; false
     ; false
     ; true
     ; true
     ; true
     ; true
     ; false
     ; true
     ; true
     ; false
     ; true
     ; false
    |]
  in
  let curve, _ = recall_precision_curve ~scores ~labels in
  let recall, precision = Array.unzip curve in
  let ap = average_precision ~recall ~precision in
  Float.robustly_compare ap 0.8783170534965226 = 0
;;

module Test = struct
  let rocr_pos =
    [| 0.612547843
     ; 0.364270971
     ; 0.244415489
     ; 0.970641299
     ; 0.890172812
     ; 0.781781371
     ; 0.716680598
     ; 0.547983407
     ; 0.628095575
     ; 0.744769966
     ; 0.657732644
     ; 0.890078186
     ; 0.984667270
     ; 0.014823599
     ; 0.543533783
     ; 0.701561487
     ; 0.715459280
     ; 0.714985914
     ; 0.911723615
     ; 0.757325590
     ; 0.529402244
     ; 0.589909284
     ; 0.326672910
     ; 0.879459891
     ; 0.230157183
     ; 0.876086217
     ; 0.353281048
     ; 0.703293499
     ; 0.627012496
     ; 0.665444679
     ; 0.536339509
     ; 0.623494622
     ; 0.885179651
     ; 0.932159806
     ; 0.858876675
     ; 0.694457482
     ; 0.517308606
     ; 0.865639036
     ; 0.005422562
     ; 0.772728821
     ; 0.277656869
     ; 0.133257805
     ; 0.531958184
     ; 0.717845453
     ; 0.537091350
     ; 0.930846938
     ; 0.663367560
     ; 0.844415442
     ; 0.943432189
     ; 0.598162949
     ; 0.834803976
     ; 0.912325837
     ; 0.642933593
     ; 0.586857799
     ; 0.700501359
     ; 0.531464015
     ; 0.938583020
     ; 0.531006532
     ; 0.785213140
     ; 0.905121019
     ; 0.748438143
     ; 0.842974300
     ; 0.835981859
     ; 0.991096434
     ; 0.757364019
     ; 0.773336236
     ; 0.110241034
     ; 0.984599159
     ; 0.253271061
     ; 0.697235328
     ; 0.620501132
     ; 0.814586047
     ; 0.698826511
     ; 0.658692553
     ; 0.501489336
     ; 0.746588080
     ; 0.579511087
     ; 0.770178504
     ; 0.537336015
     ; 0.790240205
     ; 0.883431431
     ; 0.745110673
     ; 0.012653524
     ; 0.868331219
     ; 0.540221346
     ; 0.567043171
     ; 0.806543942
     ; 0.336315317
     ; 0.268138293
     ; 0.728536415
     ; 0.739554341
     ; 0.858970526
     ; 0.606960209
    |]
  ;;

  let rocr_neg =
    [| 0.432136142
     ; 0.140291078
     ; 0.384895941
     ; 0.868751832
     ; 0.360168796
     ; 0.385240464
     ; 0.423739359
     ; 0.101699993
     ; 0.490119891
     ; 0.072369921
     ; 0.172741714
     ; 0.105722115
     ; 0.945548941
     ; 0.360180429
     ; 0.448687336
     ; 0.292368449
     ; 0.120604738
     ; 0.319672178
     ; 0.090988280
     ; 0.257402979
     ; 0.708412104
     ; 0.086546283
     ; 0.362693564
     ; 0.779771989
     ; 0.212014560
     ; 0.689075677
     ; 0.240911145
     ; 0.402801992
     ; 0.134794140
     ; 0.120473353
     ; 0.353777439
     ; 0.408939895
     ; 0.265686095
     ; 0.248500489
     ; 0.491735594
     ; 0.151350957
     ; 0.496513160
     ; 0.123504905
     ; 0.499788081
     ; 0.310718619
     ; 0.907651100
     ; 0.340078180
     ; 0.195097957
     ; 0.371936985
     ; 0.419560072
     ; 0.018527600
     ; 0.539086009
     ; 0.703885141
     ; 0.348213542
     ; 0.458674210
     ; 0.059045866
     ; 0.083685883
     ; 0.429650397
     ; 0.212404891
     ; 0.083048377
     ; 0.468610247
     ; 0.393378108
     ; 0.349540913
     ; 0.194398425
     ; 0.959417835
     ; 0.211378771
     ; 0.576836208
     ; 0.380396459
     ; 0.161874325
     ; 0.392173971
     ; 0.122284044
     ; 0.180631658
     ; 0.085993218
     ; 0.060413627
     ; 0.084254795
     ; 0.448484671
     ; 0.605235403
     ; 0.364288579
     ; 0.492596896
     ; 0.488179708
     ; 0.259278968
     ; 0.288258273
     ; 0.040906997
     ; 0.760726142
     ; 0.300973098
     ; 0.378092079
     ; 0.016694412
     ; 0.470206008
     ; 0.239143340
     ; 0.050999138
     ; 0.088450984
     ; 0.107031842
     ; 0.480100183
     ; 0.336592126
     ; 0.118555284
     ; 0.233160827
     ; 0.461150807
     ; 0.370549294
     ; 0.463227453
     ; 0.007746305
     ; 0.439399995
     ; 0.035815400
     ; 0.248707470
     ; 0.696702150
     ; 0.081439129
     ; 0.126480399
     ; 0.636728451
     ; 0.030235062
     ; 0.983494405
     ; 0.522384507
     ; 0.383807972
     ; 0.138387070
    |]
  ;;

  let scores = Array.append rocr_pos rocr_neg

  let labels =
    Array.append
      (Array.map rocr_pos ~f:(fun _ -> true))
      (Array.map rocr_neg ~f:(fun _ -> false))
  ;;

  let assert_float_equal ?msg x y =
    printf
      "%s: %s = %s: %b\n"
      (Option.value msg ~default:"")
      (Float.to_string x)
      (Float.to_string y)
      Float.(abs (x - y) < 0.00001)
  ;;

  (* let p x = BatArray.print (BatTuple.Tuple2.print BatFloat.print BatFloat.print) BatIO.stdout x *)

  let%expect_test "test_empty_data" =
    let _, auc = roc_curve ~scores:[||] ~labels:[||] in
    printf "%s: %b\n" "Test with empty data" (Float.is_nan auc);
    [%expect {| Test with empty data: true |}]
  ;;

  let%expect_test "test_2_points_good" =
    let _, auc = roc_curve ~scores:[| 0.; 2. |] ~labels:[| false; true |] in
    assert_float_equal ~msg:"Test with two points and a good classifier" 1. auc;
    [%expect {| Test with two points and a good classifier: 1. = 1.: true |}]
  ;;

  let%expect_test "test_2_points_bad" =
    let _, auc = roc_curve ~scores:[| 0.; 2. |] ~labels:[| true; false |] in
    assert_float_equal ~msg:"Test with two points and bad classifier" 0. auc;
    [%expect {| Test with two points and bad classifier: 0. = 0.: true |}]
  ;;

  let%expect_test "test_against_rocr" =
    assert_float_equal
      ~msg:"Test against ROCR failed"
      (snd (roc_curve ~scores ~labels))
      0.8341875;
    [%expect {| Test against ROCR failed: 0.83418751884232678 = 0.8341875: true |}]
  ;;
end
