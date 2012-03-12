(********************************************************************)
(* Antelatex - Preprocessing                                        *)
(********************************************************************)
(* $Time-stamp: <Malo - 2012>$ *)

(** Preprocessing between an input channel and output channel 
*)

open Common
open Io

let debug = debug "Preprocessing"
let fulldebug = fulldebug "Preprocessing"

type preproc = string * ((string * (string * string)) *(string -> info -> string))


let apply (pre:preproc list) (chan:chan) =
  let (cin,cout) = chan in
  let str = Stream.of_channel cin in 
  let sections = List.map 
    (function (b,((e,(l,r)),f)) -> 
      (string_to_charlist b,((string_to_charlist e,(l,r)),f))) pre in
(*  let sec = List.map 
    (function ((b,e),f) -> (b,(e,f))) pre in*)
  let tmp = Buffer.create 10 in
  let rec process_out current () =
    let beg = Stream.npeek 2 str in
    if List.mem_assoc beg sections
    then 
      let () = Stream.junk str; Stream.junk str in 
      let next = posaddchar current 2 in
      process_tmp current next (List.assoc beg sections)
    else if Stream.peek str = None
    then ()
    else 
      let next = (if Stream.peek str = Some '\n'
        then newline current else posaddchar current 1) in
      let () = outc chan (Stream.next str) in
      process_out next ()
  and process_tmp start current ((en,(l,r)),f) =
    let fi = Stream.npeek 2 str in
    if fi = en
    then 
      let () = Stream.junk str; Stream.junk str in 
      let s = Buffer.contents tmp in
      let () = Buffer.clear tmp in
      let _ = debug (Printf.sprintf "Transforming text from %s"
                       (info_to_string (start,current))) in
      let next = posaddchar current 2 in
      let r = l^(f s ((posaddchar start 2),next))^r in
      let () = outs chan r in
      process_out next ()
    else if Stream.peek str = None
    then 
      let s = Buffer.contents tmp in
      let () = Buffer.clear tmp in
      let r = l^(f s ((posaddchar start 2),current))^r in
      let () = outs chan r in
      ()
    else 
      let next = (if Stream.peek str = Some '\n'
        then newline current else posaddchar current 1) in
      let () = Buffer.add_char tmp (Stream.next str) in
      process_tmp start next ((en,(l,r)),f)
  in
  process_out (0,0) ()
(** [transform chan] applies the preprocessors that have been registered to
    on the [chan].
*)
