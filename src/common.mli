(********************************************************************)
(* Antelatex - Common                                               *)
(********************************************************************)
(* $Time-stamp: <Malo - 2012>$ *)

val debug : string -> string -> unit
val fulldebug : string -> string -> unit

val switch_debug_on : unit -> unit
val switch_fulldebug_on : unit -> unit

type pos = int * int
type info = pos * pos

val info_to_string : info -> string

val newline : pos -> pos
val posaddchar : pos -> int -> pos

exception Parse_error of string*info

val string_to_charlist : string -> char list
val subst : char -> string -> string -> string
