(** Buffered transforms. A buffered transform represents a method for
    converting a stream of [input]s to a stream of [output]s. However,
    [input]s can also be buffered, i.e. you can feed [input]s to the
    transform and pull out [output]s later. There is no requirement
    that 1 input produces exactly 1 output. It is common that multiple
    input values are needed to construct a single output, and vice
    versa.

    Buffered transforms serve as a general method for working with
    streams of data and flexibly composing mappings from [input]s to
    [output]s. The buffering aspect supports asynchronous programming
    interfaces. Parsers and printers throughout Biocaml are
    implemented with this module whenever possible.

    Often mappings need to account for errors, e.g. an input string
    cannot be converted to an integer. Several methods below
    explicitly support buffered transforms where the output type is a
    [result].
*)

(** Type of a buffered transform converting ['input]s to
    ['output]s. *)
type ('input, 'output) t

(** Exception thrown when [feed] is called on a transform after it
    has been [stop]ped. *)
exception Feeding_stopped_transform of string

(** [make ~feed ~next ()] creates a transform that can be
    fed with [feed] and read from with [next].

    - [feed input] should store [input] in a buffer, which is
    presumably a shared state also available to [next].

    - [next stopped] should remove values from the buffer, convert it
    to an [`output] and return this output, or return [`not_ready] if
    there are not enough buffered inputs to create an output value, or
    return [`end_of_stream] if the buffer has been stopped, as
    determined by the supplied argument, and there is no more
    input.

    Depending on the specifics of the transform, it may be the case
    that the buffer has been stopped but there is not enough input to
    create an output value. It is the caller's choice how to handle
    this or any other kind of error, e.g. make the return type a
    [result].

    - [name] an optional name for the transform that will be used in
    error messages. *)
val make:
  ?name:string ->
  feed: ('input -> unit) ->
  next: (bool -> [ `output of 'output | `end_of_stream | `not_ready ]) ->
  unit ->
  ('input, 'output) t

(** [feed t i] stores [i] into the buffered transform.

    @raise Feeding_stopped_transform [name] if called on a [t]
    that has been [stop]ped. *)
val feed: ('input, 'output) t -> 'input -> unit

(** [next t] returns an output value if possible, [`not_ready] if [t]
    needs to be fed with more input before it can produce an output,
    or [`end_of_stream] if [t] has been stopped and has no more
    data. *)
val next: ('input, 'output) t -> [ `output of 'output | `end_of_stream | `not_ready ]

(** [stop t] declares [t] to be stopped, which means subsequent calls to:

    - [feed t _] will raise [Feeding_stopped_transform]. Feeding
    a stopped transform is not allowed.

    - [next t] will eventually return [`end_of_stream], not
    necessarily the immediate next call as there may still be
    buffered values available for output. *)
val stop: ('input, 'output) t -> unit

(** [name t] returns the name of [t]. *)
val name: ('input, 'output) t -> string option

(** [identity ()] returns a transform that simply returns its inputs
    as outputs without modification (it can be seen as a
    simple [Queue.t]). *)
val identity: ?name:string -> unit -> ('a, 'a) t

(** [of_function f] is like [identity ()] but the transform outputs
    are passed to the function [f]. *)
val of_function: ?name:string -> ('a -> 'b) -> ('a, 'b) t

(** [to_stream_fun t] returns a function [f] that behaves like
    [t] but the inputs and outputs are on standard OCaml streams. *)
val to_stream_fun:
  ('input, 'output) t -> ('input Stream.t -> 'output Stream.t)

(** [in_channel_strings_to_stream ic t] returns a stream of ['output]s
    given a transform [t] that knows how to produce ['output]s from
    strings. The strings are read from the in_channel. *)
val in_channel_strings_to_stream :
  ?buffer_size:int -> in_channel -> (string, 'output) t -> 'output Stream.t

(** [stream_to_out_channel xs t oc] consumes a stream of ['input]s
    using [t] to transform them into strings, which are then written
    on the out_channel [oc]. *)
val stream_to_out_channel :
  'input Stream.t -> ('input, string) t -> out_channel ->  unit

(** {2 Compose}

    Buffered transforms are mutable and one should not expect nice
    mathematical properties from composing them. The intention here is
    to provide building blocks that allow the creation of more complex
    transforms from simpler ones. Only the final resultant transform
    should be used. Feeding/reading the transforms being composed is
    likely to lead to violations of the stated behavior of the above
    operations. *)

(** [on_input f t] returns a transform that converts its inputs with
    [f] and feeds the results to [t]. *)
val on_input: ('b, 'c) t -> f:('a -> 'b) -> ('a, 'c) t

(** [on_output t f] returns a transform that behaves like [t] except
    the outputs are first converted by [f]. *)
val on_output: ('a, 'b) t -> f:('b -> 'c) -> ('a, 'c) t

val compose: ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
(** [compose t u] composes [t] and [u].
    {figure src/doc/figures/transform_compose.svg 50%
    “Compose” two transforms} *)

val mix : ('a1, 'b1) t -> ('a2, 'b2) t ->
  ('a1 * 'a2, [ `both of 'b1 * 'b2 | `left of 'b1 | `right of 'b2 ]) t
(** [mix t u] returns a transform that takes as input a pair of the
    inputs expected by [t] and [u], and outputs
    either [both] outputs, or, when one transform has reach the end of
    its stream, the output of the remaining one (as [`left _] or
    [`right _]).
    {figure src/doc/figures/transform_mix.svg 50%
    “Mix” the inputs of two transforms}
*)

val filter_compose:
  ('il, 'ol) t -> ('ir, 'our) t ->
  destruct:('ol -> [`transform of 'ir | `bypass of 'filtered]) ->
  reconstruct:([`bypassed of 'filtered | `transformed of 'our] -> 'result) ->
  ('il, 'result) t
(** [filter_compose t u ~destruct ~reconstruct] produces a transform
    that feeds a filtered subset of [t]s outputs to [u]. Only those
    outputs [ol] of [t] for which [destruct ol] returns [`transform] are
    passed on to [u]. The filterd out values are combined with [u]'s
    output using [reconstruct].
    {figure src/doc/figures/transform_filter_compose.svg 50%
    “Compose” two transforms with a filtering function }
*)

val split_and_merge:
  ('il, 'ol) t -> ('ir, 'our) t ->
  split:('input -> [`left of 'il | `right of 'ir]) ->
  merge:([`left of 'ol | `right of 'our] -> 'output) ->
  ('input, 'output) t
(** [split_and_merge t u ~split ~merge] returns a transform whose
    input is split using [split], passing the result either to [t] or [u],
    and then the outputs of [t] and [u] are combined using [merge]. There
    is no guarantee about the order in which the inputs are fed to [t] and
    [u] (it depends on the buffering done by the individual input
    transforms).
    {figure src/doc/figures/transform_split_merge.svg 50%
    “Split” the inputs of two transforms and “merge” the outputs }     *)


(** {2 result Outputs}

    Operations analogous to those above, but for transforms whose
    output types are [result]s. *)

(** Like {!make} but the output is a [result]. Also,
    {!stop} is automatically called when an error occurs. *)
val make_result:
  ?name:string ->
  feed: ('input -> unit) ->
  next: (bool -> [ `output of ('a, 'b) result | `end_of_stream | `not_ready ]) ->
  unit ->
  ('input, ('a, 'b) result) t

(** Like [on_output] but on the successful  part of the {i output}. *)
val on_ok: ('input, ('ok, 'error) result) t ->
  f:('ok -> 'still_ok) ->
  ('input, ('still_ok, 'error) result) t

(** Like [on_output] but on the erroneous  part of the {i output}. *)
val on_error: ('input, ('ok, 'error) result) t ->
  f:('error -> 'another_errror) ->
  ('input, ('ok, 'another_errror) result) t


val compose_results:
  on_error:([`left of 'error_left | `right of 'error_right ] -> 'error) ->
  ( 'input_left, ('middle, 'error_left) result) t ->
  ( 'middle, ('output_right, 'error_right) result) t ->
  ( 'input_left, ('output_right, 'error) result) t
(** [compose_results t u] is like {!compose} but for transforms returning
    [result]s. The [on_error] function specifies how errors in [t]
    or [u] should be converted into those in the resultant
    transform.
    {figure src/doc/figures/transform_compose_results.svg 50%
    “Compose” two transforms which may fail}
*)

val compose_results_merge_error:
  ('a, ('b, 'el) result) t ->
  ('b, ('d, 'er) result) t ->
  ('a, ('d, [ `left of 'el | `right of 'er ]) result) t
(** Like {!compose_results} but with a pre-specified [on_error]
    function. *)

val compose_result_left:
  ( 'input_left, ('middle, 'error) result) t ->
  ( 'middle, 'output_right) t ->
  ( 'input_left, ('output_right, 'error) result) t
(** Like {!compose_results} but only the first transform returns
    [result]s.
    {figure src/doc/figures/transform_compose_result_left.svg 50%
    “Compose” two transforms when only the first one may fail }
*)

(** {2 Communication with other libraries} *)

(** Generic transform type. *)
class type ['input, 'output] object_t = object
  method next: [ `output of 'output | `end_of_stream | `not_ready ]
  method feed:  'input -> unit
  method stop: unit
end
val to_object: ('a, 'b) t -> ('a, 'b) object_t
val of_object: ('a, 'b) object_t -> ('a, 'b) t

(** {2 Low-level API} *)

(** The most general way to make a transform. All make functions above
    are implemented with this one. *)
val make_general:
  ?name:string ->
  next: (unit -> [ `output of 'output | `end_of_stream | `not_ready ]) ->
  feed: ('input -> unit) ->
  stop: (unit -> unit) ->
  unit ->
  ('input, 'output) t
