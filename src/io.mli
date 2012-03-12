(********************************************************************)
(* Antelatex - IO                                                   *)
(********************************************************************)
(* $Time-stamp: <Malo - 2012>$ *)

(** Creating, reading and writing on channels
*)

type chan = in_channel * out_channel 

val create_chan : string -> string -> chan

val close_chan : chan -> unit

val outc : chan -> char -> unit
val outs : chan -> string -> unit
val inc : chan -> char
