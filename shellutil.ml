(* arch-tag: Shell utilities
* Copyright (c) 2004 John Goerzen
*)

open Shell;;

let run prog args =
  call [cmd prog args];;

let runnoout prog args =
  let stdoutbuf = Buffer.create 10 in
  let stderrbuf = Buffer.create 10 in
  call ~stdout:(to_buffer stdoutbuf) ~stderr:(to_buffer stderrbuf)
   [cmd prog args];;
