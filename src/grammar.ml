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
  let case l n s r =
    let _ = fulldebug_gram (Printf.sprintf "Parsing grammar file: arity %d" l) in
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
    else Scanf.sscanf s "{{%s@}{%s@}{%s@}{%s@}}%s" aux
  and aux dl dr sl sr q =
    if String.length dl <> 2 || String.length dr <> 2 then
      failwith "Delimiters should be two characters long";
    let _ = fulldebug_gram 
      (Printf.sprintf 
         "Adding transformation: between %S and %S, bound characters %S %S"
         dl dr sl sr) in
    (dl,(dr,(sl,sr)))::(parse_def q) in
  try
    while true do
      let s = input_line inp in
      if s.[0] = '{' then begin
        let _ = fulldebug_gram 
          (Printf.sprintf "Parsing grammar file: new transformation detected") in
        let table = Hashtbl.create 10 in
        reg:= table;
        gra:=(parse_def s,table)::(!gra)
      end
      else
        Scanf.sscanf s "%l%d {%s@} = %s" case
    done;
    !gra
  with End_of_file -> (
    let _ = debug_gram (Printf.sprintf "Parsing grammar file done.") in
    !gra) 
