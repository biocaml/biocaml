include Future.S
  with type 'a Deferred.t = 'a
  and type 'a Pipe.Reader.t = 'a Stream.t
  and type Reader.t = In_channel.t
  and type Writer.t = Out_channel.t
