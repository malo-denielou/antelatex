(********************************************************************)
(* Antelatex - Preprocessing                                        *)
(********************************************************************)
(* $Time-stamp: <Malo - 2012>$ *)

(** Preprocessing between an input channel and output channel 
*)


type preproc = 
    string * 
      ((string * (string * string)) * 
          (string -> Common.info -> string))

val apply : preproc list -> Io.chan -> unit
