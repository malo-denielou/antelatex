(********************************************************************)
(* Antelatex - Common                                               *)
(********************************************************************)
(* $Time-stamp: <Malo - 2012>$ *)

let debug_var = ref false
let fulldebug_var = ref false

let debug modul msg =
  if !debug_var then begin
    print_string ("["^modul^"] ");
    print_string (msg);
    print_newline ();
    flush stdout
  end
  else ()

let fulldebug modul msg =
  if !fulldebug_var then begin
    print_string ("["^modul^"] ");
    print_string (msg);
    print_newline ();
    flush stdout
  end
  else ()

let switch_debug_on () = debug_var := true
let switch_fulldebug_on () = debug_var := true;fulldebug_var := true

(* Positionning *)

type pos = int * int
type info = pos * pos

let bogusInfo = ((0,0),(0,0))

let info_to_string ((l1,c1),(l2,c2)) =
  let s = Printf.sprintf "line %d, char %d, to line %d, char %d" l1 c1 l2 c2 in
  s

let newline (l,c) = (l+1,0)
let posaddchar (l,c) n = (l,c+n)

(* Errors *)
exception Parse_error of string*info


(* String manipulation *)

let index s p =
  let c = p.[0] in
  let rec index_from i =
      let r = String.index_from s i c in
      if String.sub s r (String.length p) = p then
        r
      else
        index_from (r+1) in

  try index_from 0
  with _ -> max_int

let string_to_charlist s =
  let l = ref [] in
  let () = String.iter (function c -> l:=c::!l) s in
  List.rev !l

let subst c e s =
  let rec subst_aux acc s =
    try 
      let i = String.index s '#' in
      if s.[i+1] = c 
      then 
        let d = String.sub s 0 i in
        let rest = (String.sub s (i+2) (String.length s - i - 2)) in
        (subst_aux (acc^d^e) rest)
      else
        let d = String.sub s 0 (i+2) in
        (subst_aux (acc^d) (String.sub s (i+2) (String.length s - i - 2)))
      with Not_found -> 
        acc^s
  in
  subst_aux "" s

let guess_arity s = 
  let n = String.length s in
  let t = ref false in
  let m = ref 0 in
  for i = 0 to n-1 do
    let c = int_of_char s.[i] in
    if c = int_of_char '#' then t:= true
    else if !t && (49 <= c && c <= 52)
    then (t:=false ; m:=max (!m) (c-48))
  done;
  !m
