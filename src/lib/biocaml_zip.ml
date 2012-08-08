open Biocaml_internal_pervasives
  
type unzip_error =
[ `garbage_at_end_of_compressed_data of string
| `wrong_gzip_header of
    [ `compression_method | `flags | `magic_number ] * int ]

open Result
let try_skip_gzip_header_exn buffer =
  let bytes_read = ref 0 in
  let ignore_bytes n = bytes_read := !bytes_read + n in
  let assert_byte f error =
    let byte = (Char.to_int buffer.[!bytes_read]) in
    if f byte 
    then (incr bytes_read; return byte)
    else fail (`wrong_gzip_header (error, byte)) in
  let assert_byte_eq b error = assert_byte ((=) b) error in
  let read_byte () = 
    let byte = (Char.to_int buffer.[!bytes_read]) in
    (incr bytes_read; return byte) in
  let rec skip_null_terminated () =
    read_byte () >>= fun x ->
    if x = 0 then return () else skip_null_terminated () in
  assert_byte_eq 0x1f `magic_number >>= fun _ ->
  assert_byte_eq 0x8b `magic_number >>= fun _ ->
  assert_byte_eq 8 `compression_method >>= fun _ ->
  assert_byte (fun b -> b land 0xe0 = 0) `flags >>= fun flags ->
  ignore_bytes 6;
  (if flags land 0x04 <> 0 then (
    (* Skip extra data *)
    read_byte () >>= fun len1 ->
    read_byte () >>= fun len2 ->
    ignore_bytes (len1 + len2 lsl 8);
    return ()
   ) else return ())
  >>= fun () ->
  (if flags land 0x08 <> 0 then skip_null_terminated () else return ())
  >>= fun () ->
  (if flags land 0x10 <> 0 then skip_null_terminated () else return ())
  >>= fun () ->
  if flags land 0x02 <> 0 then begin
    (* Skip header CRC *)
    ignore_bytes 2;
  end;
  return !bytes_read

let inflate_as_much_as_possible in_buffer buffer
    zstream zlib_write_buffer zlib_buffer_size format = 
  let len = String.length buffer in
  let try_to_output used_out =
    if used_out > 0 then
      `output (String.sub zlib_write_buffer 0 used_out)
    else
      `not_ready in
  let (finished, used_in, used_out) =
    Zlib.inflate zstream buffer 0 len
      zlib_write_buffer 0 zlib_buffer_size
      Zlib.Z_SYNC_FLUSH in
  if used_in < len then (
    if finished then (
      match format with
      | `gzip when len - used_in = 8 ->
        try_to_output used_out
      | _ ->
        `error (`garbage_at_end_of_compressed_data
                   String.(sub buffer used_in (len - used_in)))
    ) else (
      Buffer.add_string in_buffer
        String.(sub buffer used_in (len - used_in));
      try_to_output used_out
    )
  ) else try_to_output used_out
  
let unzip ?(format=`raw) ?(zlib_buffer_size=4096) () =
  let zstream =  Zlib.inflate_init false in
  let in_buffer = Buffer.create 42 in
  let zlib_write_buffer = String.create zlib_buffer_size  in
  let current_state =
    ref (match format with `gzip -> `gzip_header | `raw -> `inflate) in
  Biocaml_transform.make_stoppable ()
    ~feed:(fun string -> Buffer.add_string in_buffer string;)
    ~next:(fun stopped ->
      let buffered = Buffer.contents in_buffer in
      let len = String.length buffered in
      Buffer.clear in_buffer;
      if len > 0
      then begin
        begin match !current_state with
        | `inflate ->
          inflate_as_much_as_possible in_buffer buffered
            zstream zlib_write_buffer zlib_buffer_size  format
        | `gzip_header ->
          eprintf "Header parsed!\n%!";
          begin
            try
              match try_skip_gzip_header_exn buffered with
              | Ok bytes_read -> 
                current_state := `inflate;
                inflate_as_much_as_possible in_buffer
                  String.(sub buffered bytes_read (len - bytes_read))
                  zstream zlib_write_buffer zlib_buffer_size format
              | Error e -> `error e
            with 
              e -> Buffer.add_string in_buffer buffered; `not_ready
          end
        end
      end
      else (if stopped then `end_of_stream else `not_ready)
    )      

