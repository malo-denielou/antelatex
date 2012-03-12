(********************************************************************)
(* Antelatex - IO                                                   *)
(********************************************************************)
(* $Time-stamp: <Malo - 2012>$ *)

(** Creating, reading and writing on channels
*)

type chan = in_channel * out_channel 
(** Combination of input channel and output channel *)

let create_chan source dest =
  let cin = open_in source in
  let cout = 
    open_out_gen [Open_text;Open_creat;Open_trunc;Open_wronly] 0o666 dest in
  (cin,cout)
(** [IO.create_chan source dest] creates a [chan] from [source] and [dest].
    [source] is the filename of the file to process. [dest] is the filename
    of the destination file.
*)

let close_chan (cin,cout) =
  close_in cin ; close_out cout
(** closing the channel *)

let outc (cin,cout) c = 
  output_char cout c
(** Outputing a character on a channel *)

let outs (cin,cout) c = 
  output_string cout c
(** Outputing a string on a channel *)

let inc (cin,cout) = 
  input_char cin
(** Reading a character from a channel *)

