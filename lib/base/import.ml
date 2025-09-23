module Result_list = struct
  let mapi (type error) l ~(f : int -> 'a -> ('b, error) Result.t) =
    let module M = struct
      exception E of error

      let the_fun () =
        let run () =
          List.mapi l ~f:(fun i x ->
            match f i x with
            | Ok o -> o
            | Error e -> raise (E e))
        in
        try Ok (run ()) with
        | E e -> Error e
      ;;
    end
    in
    M.the_fun ()
  ;;

  let map l ~f = mapi l ~f:(fun _ x -> f x)
end

module String2 = struct
  let chunks_of_rev ~max_length str : string list =
    let max_length = Int.max 1 max_length in
    let n = String.length str in
    let rec loop pos acc =
      let remaining = n - pos in
      match remaining > 0 with
      | false -> acc
      | true -> (
        match remaining <= max_length with
        | true -> String.sub str ~pos ~len:remaining :: acc
        | false ->
          let chunk = String.sub str ~pos ~len:max_length in
          loop (pos + max_length) (chunk :: acc))
    in
    loop 0 []
  ;;

  let chunks_of str ~max_length = chunks_of_rev str ~max_length |> List.rev

  module Test = struct
    open Expect_test_helpers_base

    let%expect_test "chunks_of basic functionality" =
      let data =
        [ ("foobar", 3), "basic chunking"
        ; ("hello", 7), "chunk size larger than string"
        ; ("a", 1), "single character string"
        ; ("hello", 1), "chunk size of 1"
        ; ("abcdefghij", 10), "chunk size equals string length"
        ; ("abcdefghijk", 5), "string length not multiple of chunk size"
        ; ("", 3), "empty string"
        ; ("hello", 0), "max_length of 0"
        ; ("hello", -1), "negative max_length"
        ]
      in
      let test ((str, max_length), msg) =
        let result = chunks_of str ~max_length in
        print_string
          (sprintf
             "✅ TEST: %s\nINPUT: str=%S, max_length=%d\nRESULT: %s\n\n"
             msg
             str
             max_length
             (result |> List.sexp_of_t String.sexp_of_t |> Sexp.to_string_hum))
      in
      List.iter data ~f:test;
      [%expect
        {|
        ✅ TEST: basic chunking
        INPUT: str="foobar", max_length=3
        RESULT: (foo bar)

        ✅ TEST: chunk size larger than string
        INPUT: str="hello", max_length=7
        RESULT: (hello)

        ✅ TEST: single character string
        INPUT: str="a", max_length=1
        RESULT: (a)

        ✅ TEST: chunk size of 1
        INPUT: str="hello", max_length=1
        RESULT: (h e l l o)

        ✅ TEST: chunk size equals string length
        INPUT: str="abcdefghij", max_length=10
        RESULT: (abcdefghij)

        ✅ TEST: string length not multiple of chunk size
        INPUT: str="abcdefghijk", max_length=5
        RESULT: (abcde fghij k)

        ✅ TEST: empty string
        INPUT: str="", max_length=3
        RESULT: ()

        ✅ TEST: max_length of 0
        INPUT: str="hello", max_length=0
        RESULT: (h e l l o)

        ✅ TEST: negative max_length
        INPUT: str="hello", max_length=-1
        RESULT: (h e l l o)
        |}]
    ;;
  end
end
