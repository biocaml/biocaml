include BytesLabels

(* This is adapted from janestreet's Base *)
let split str ~on:c =
  let len = String.length str in
  let rec loop acc last_pos pos =
    if pos = -1 then
      sub str ~pos:0 ~len:last_pos :: acc
    else
    if str.[pos] = c then
      let pos1 = pos + 1 in
      let sub_str = sub str ~pos:pos1 ~len:(last_pos - pos1) in
      loop (sub_str :: acc) pos (pos - 1)
    else loop acc last_pos (pos - 1)
  in
  loop [] len (len - 1)
