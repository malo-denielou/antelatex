(********************************************************************)
(* Antelatex - Grammar                                              *)
(********************************************************************)
(* $Time-stamp: <Malo - 2012>$ *)


type sep = Cur | Par | Bra

type expr =
  | S of string
  | F1 of (string -> string)
  | F2 of (string -> string -> string)
  | F3 of (string -> string -> string -> string)
  | F4 of (string -> string -> string -> string -> string)

type grammar = ((string * (string *(string * string))) list 
                * (string,expr) Hashtbl.t) list

val grammar : string -> grammar
