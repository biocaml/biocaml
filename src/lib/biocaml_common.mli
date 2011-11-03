(** Commonly needed values; the 'Pervasives' of Biocaml. *)


exception PosError of string * int * int * exn
  (** An error exception that wraps an underlying exception (the last
      value) with information about the position where that exception
      occurred. The position is given by the first three values:

      - Name of the source where underlying exception occurred,
      e.g. file name or url. Empty string indicates unknown source.

      - Line number within the source where the exception occurred. If
      the source is not text, this can be some other indication of
      which item caused the exception, e.g. the nth item in an
      enum. Negative value indicates unknown location.

      - Character position within the previous line that caused the
      exception. Negative value indicates unknown location. *)
