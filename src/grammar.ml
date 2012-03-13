(********************************************************************)
(* Antelatex - Grammar                                              *)
(********************************************************************)
(* $Time-stamp: <Malo - 2012>$ *)

open Common

let debug_gram = debug "Grammar"
let fulldebug_gram = fulldebug "Grammar"

type sep = Cur | Par | Bra

type expr =
  | S of string
  | F1 of (string -> string)
  | F2 of (string -> string -> string)
  | F3 of (string -> string -> string -> string)
  | F4 of (string -> string -> string -> string -> string)

type grammar = ((string * (string *(string * string))) list 
                * (string,expr) Hashtbl.t) list


let grammar (filename:string) : grammar =
  let gra : grammar ref = ref [] in

  let reg : (string,expr) Hashtbl.t ref
      = ref (Hashtbl.create 1) in
  
  let add s e = 
    let _ = fulldebug_gram (Printf.sprintf "Adding keyword %s" s) in
    Hashtbl.add !reg s e in

  let inp = open_in filename in

  let line = ref 0 in

  let case n s r =
    let n = if n="" then guess_arity r else int_of_string n in
    let _ = fulldebug_gram 
      (Printf.sprintf 
         "Parsing grammar file line %d: arity %d" !line n) in
    match n with
        0 -> add s (S r)
      | 1 -> add s (F1 (fun x -> (subst '1' x r)))
      | 2 -> add s (F2 (fun x y -> (subst '1' x (subst '2' y r))))
      | 3 -> add s (F3 (fun x y z -> (subst '1' x (subst '2' y (subst '3' z r)))))
      | 4 -> add s 
        (F4 (fun x y z w -> (subst '1' x (subst '2' y (subst '3' z (subst '4' w r))))))
      | _ -> assert false
  in
  let _ = debug_gram (Printf.sprintf "Parsing grammar file %s" filename) in

  let rec parse_def s =
    if s = "" then []
    else Scanf.sscanf s "%s@{{%s@}{%s@}{%s@}{%s@}}%s" aux
  and aux _ dl dr sl sr q =
    if String.length dl <> 2 || String.length dr <> 2 then
      failwith "Delimiters should be two characters long";
    let _ = fulldebug_gram 
      (Printf.sprintf 
         "Adding transformation line %d: between %S and %S, bound characters %S %S"
         !line dl dr sl sr) in
    (dl,(dr,(sl,sr)))::(parse_def q) in
  try
    while true do
      let s = input_line inp in
      incr(line);
      if s.[0] = 'D' || (s.[0] = '{' && s.[1] = '{') then begin
        let _ = fulldebug_gram 
          (Printf.sprintf "Parsing grammar file: new transformation detected") in
        let table = Hashtbl.create 10 in
        reg:= table;
        gra:=(parse_def s,table)::(!gra)
      end
      else
        Scanf.sscanf s "%[0-9] {%s@} = %s" case
    done;
    !gra
  with End_of_file -> (
    let _ = debug_gram (Printf.sprintf "Parsing grammar file done.") in
    !gra) 
