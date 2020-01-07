(** I/O on Blocked GNU Zip format (BGZF) files *)

type in_channel
(** Representation of files opened for reading. *)

val open_in : string -> in_channel
(** Opens a BGZF file for reading. @raise Sys_error if the path
    given in argument does not refer to an existing file. *)

val of_in_channel : Stdlib.in_channel -> in_channel
(** Uses a regular channel to read a BGZF compressed file. *)

val close_in : in_channel -> unit
(** Closes an open file. The channel cannot be used after that
    call. *)

val dispose_in : in_channel -> unit
(** Releases the resources associated to a (BGZF) channel (it can
    thus not be used after that call), apart from the underlying
    regular channel (which can be used further). *)

val seek_in : in_channel -> Int64.t -> unit
(** [seek_in iz p] moves the current handler to the position [p], a
   so-called {i virtual file offset}, as described in paragraph 4.1.1
   of the {{:https://samtools.github.io/hts-specs/SAMv1.pdf}SAM/BAM
   format specification}. The upper 48 bits correspond to a standard
   file offset which must match a block beginning, and the lower 16
   correspond to an offset in the {i uncompressed} data of the
   block. *)

val virtual_offset : in_channel -> Int64.t
(** [virtual_offset iz] provides the current position in the file
   using the same encoding than for [seek_in]. *)

exception Error of string
(** Exception signaling an incorrect format while reading data from an
    open file. All input functions may raise this exception. *)

val input_char : in_channel -> char
val input_u8 : in_channel -> int
val input_s8 : in_channel -> int
val input_u16 : in_channel -> int
val input_s16 : in_channel -> int
val input_s32 : in_channel -> int32

val input: in_channel -> string -> int -> int -> int
(** [input ic buf pos len] reads at most [len] characters in file
    [ic], stores them in string [buf] at position [pos], and returns
    the number of characters actually read. *)

(* FIXME: should raise End_of_file if not enough characters are available! *)
val really_input : in_channel -> string -> int -> int -> unit
(** Same as [input] but reads exactly [len] characters. @raise
    End_of_file if there are less than [len] characters available.  *)

val input_string : in_channel -> int -> string
(** Same as [really_input] but returns the result in a fresh
    string. *)

val with_file_in : string -> f:(in_channel -> 'a) -> 'a
(** [with_file_in fn ~f] opens a channel for reading, pass it to [f],
    and returns the result after having closed the channel. If the
    call to [f] raises an exception, it is caught and the channel is
    closed before the exception is re-raised. *)


type out_channel
(** Representation of files opened for writing.*)

val open_out : ?level:int -> string -> out_channel
(** [open_out ~level fn] opens the file at path [fn] for writing a
    BGZF-compressed file with compression level [level] (default is 6,
    legal values are 1 to 9). @raise Sys_error if [fn] does not refer
    to an existing file. @raise Invalid_arg if [level] is not between
    1 and 9. *)

val of_out_channel : ?level:int -> Stdlib.out_channel -> out_channel
(** Uses a regular channel to write a BGZF compressed file. *)

val close_out : out_channel -> unit
(** Closes a file opened for writing. The channel must not be used
    after that call. *)

val dispose_out : out_channel -> unit
(** Releases the resources associated to a (BGZF) channel (it can
    thus not be used after that call), apart from the underlying
    regular channel (which can be used further). *)


val output : out_channel -> string -> int -> int -> unit
(** [output oc buf pos len] writes [len] characters of string [buf]
    from position [pos] into the compressed file [oc]. *)

val output_char : out_channel -> char -> unit

val output_u8 : out_channel -> int -> unit
(** [output_u8 oz n] writes the 8 least significant bits onto channel
    [oz] *)

val output_s8 : out_channel -> int -> unit
(** [output_s8 oz n] writes a signed representation of [n], if [n] is
    between -128 and 127. @raise Invalid_arg if [n] is outside this
    range. *)

val output_u16 : out_channel -> int -> unit
(** [output_u16 oz n] writes the 16 least significant bits onto channel
    [oz] *)

val output_s16 : out_channel -> int -> unit
(** [output_s8 oz n] writes a signed representation of [n], if [n] is
    between -32768 and 32767. @raise Invalid_arg if [n] is outside this
    range. *)

val output_s32 : out_channel -> int32 -> unit
(** [output_s32 oz n] writes a signed representation of [n]. *)

val output_string : out_channel -> string -> unit

val with_file_out : ?level:int -> string -> f:(out_channel -> 'a) -> 'a
(** [with_file_out ~level fn ~f] opens a file for writing at
    compression level [level] (default is 6), passes the channel to
    [f] and returns the result after closing the channel. If the call
    to [f] raises an exception, it is re-raised after closing the
    channel. *)
