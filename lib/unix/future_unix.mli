include Future.S
  with type 'a Deferred.t = 'a
  and type 'a Pipe.Reader.t = 'a Stream.t
  and type Reader.t = in_channel
  and type Writer.t = out_channel
