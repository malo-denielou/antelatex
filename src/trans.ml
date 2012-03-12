(********************************************************************)
(* Antelatex - Trans                                                *)
(********************************************************************)
(* $Time-stamp: <Malo - 2012>$ *)

open Preproc
open Common
open Grammar
open Genlex

let debug = debug "Trans"
let fulldebug = fulldebug "Trans"

let parse grammar=
  let lexer = make_lexer ["{";"}";"(";")";"[";"]";",";".";";";"*"] in
  fun s info -> 
    begin
      let char_str = Stream.of_string s in
      let str = lexer char_str in
      let rec parse_one acc =
        try
          match Stream.next str with
            | Ident s -> 
              let _ = fulldebug (Printf.sprintf "Looking at keyword %s" s) in
              begin
                if Hashtbl.mem grammar s then
                  match Hashtbl.find grammar s with
                      S v -> acc^" "^v
                    | F1 _ | F2 _ | F3 _ | F4 _ -> 
                      raise (Common.Parse_error (Printf.sprintf 
                                            "%S cannot be an argument to another macro" s,
                                  (info)))
                else
                  raise (Common.Parse_error (Printf.sprintf "Unknown keyword %S" s,
                              info))
              end
            | String s -> acc^" \\mbox{"^s^"}"
            | Int n -> (string_of_int n)
            | Kwd "[" -> acc^" ["^(parse_group Bra "")
            | Kwd "{" -> acc^" {"^(parse_group Cur "")
            | Kwd "(" -> acc^" ("^(parse_group Par "")
            | Kwd ")" -> raise (Common.Parse_error (Printf.sprintf 
                                     "Unmatched closing parenthesis ')'",
                                     (info)))
            | Kwd "}" -> raise (Common.Parse_error (Printf.sprintf 
                                     "Unmatched closing brace '}'",
                                     (info)))
            | Kwd "]" -> raise (Common.Parse_error (Printf.sprintf 
                                     "Unmatched closing bracket ']'",
                                     (info)))
            | Kwd c -> acc^c
            | _ -> raise (Common.Parse_error (Printf.sprintf 
                               "Error while processing ...",
                               (info)))
        with
            Stream.Failure -> raise (Common.Parse_error (Printf.sprintf 
                                          "Missing argument",
                                          (info)))
      and parse_group sep acc =
        try
          match Stream.next str with
            | Ident s -> 
              let _ = fulldebug (Printf.sprintf "Looking at keyword %s" s) in
              begin
                if Hashtbl.mem grammar s then
                  match Hashtbl.find grammar s with
                      S v -> parse_group sep (acc^" "^v)
                    | F1 f -> 
                      let arg1 = (parse_one "") in parse_group sep (acc^" "^(f arg1))
                    | F2 f -> 
                      let arg1 = (parse_one "") in
                      let arg2 = (parse_one "") in
                      parse_group sep (acc^" "^(f arg1 arg2))
                    | F3 f -> 
                      let arg1 = (parse_one "") in
                      let arg2 = (parse_one "") in
                      let arg3 = (parse_one "") in
                      parse_group sep (acc^" "^(f arg1 arg2 arg3))
                    | F4 f -> 
                      let arg1 = (parse_one "") in
                      let arg2 = (parse_one "") in
                      let arg3 = (parse_one "") in
                      let arg4 = (parse_one "") in
                      parse_group sep (acc^" "^(f arg1 arg2 arg3 arg4))
                else
                  raise (Common.Parse_error (Printf.sprintf 
                                        "Unknown keyword %S" s, 
                              info))
              end
            | String s -> parse_group sep (acc^" \\mbox{"^s^"}")
            | Kwd "[" -> let fin = parse_group Bra "" in parse_group sep (acc^" ["^fin)
            | Kwd "{" -> let fin = parse_group Cur "" in parse_group sep (acc^" {"^fin)
            | Kwd "(" -> let fin = parse_group Par "" in parse_group sep (acc^" ("^fin)
            | Kwd ")" -> if sep = Par then acc^")" 
              else raise (Parse_error ("Parenthesis error while processing: "^s,info))
            | Kwd "}" -> if sep = Cur then acc^"}" 
              else raise (Parse_error ("Braces error while processing: "^s,info))
            | Kwd "]" -> if sep = Bra then acc^"]" 
              else raise (Parse_error ("Brackets error while processing: "^s,info))
            | Kwd c -> parse_group sep (acc^c)
            | Int n -> parse_group sep (acc^(string_of_int n))
            | _ -> raise (Parse_error ("Error while processing: "^s,info))
        with
            Stream.Failure -> raise (Common.Parse_error (Printf.sprintf 
                                          "Missing argument",
                                          (info)))
      in
      let rec inparse acc =
        try
          match Stream.next str with
            | Ident s -> 
              let _ = fulldebug (Printf.sprintf "Looking at keyword %s" s) in
              begin
                if Hashtbl.mem grammar s then
                  match Hashtbl.find grammar s with
                      S v -> inparse (acc^" "^v)
                    | F1 f -> 
                      let arg1 = (parse_one "") in inparse (acc^" "^(f arg1))
                    | F2 f -> 
                      let arg1 = (parse_one "") in
                      let arg2 = (parse_one "") in
                      inparse (acc^" "^(f arg1 arg2))
                    | F3 f -> 
                      let arg1 = (parse_one "") in
                      let arg2 = (parse_one "") in
                      let arg3 = (parse_one "") in
                      inparse (acc^" "^(f arg1 arg2 arg3))
                    | F4 f -> 
                      let arg1 = (parse_one "") in
                      let arg2 = (parse_one "") in
                      let arg3 = (parse_one "") in
                      let arg4 = (parse_one "") in
                      inparse (acc^" "^(f arg1 arg2 arg3 arg4))
                else
                  raise (Common.Parse_error (Printf.sprintf 
                                        "Unknown keyword %S" s,
                              (info)))
              end
            | String s -> inparse (acc^" \\mbox{"^s^"}")
            | Kwd "[" -> inparse (acc^" [")
            | Kwd "{" -> inparse (acc^" {")
            | Kwd "(" -> inparse (acc^" (")
            | Kwd c -> inparse (acc^c)
            | Int n -> inparse (acc^(string_of_int n))
            | Float n -> inparse (acc^(string_of_float n))
            | _ -> raise (Common.Parse_error (Printf.sprintf "Error while processing %s" s,info))
        with
            Stream.Failure -> acc
      in
      let s = inparse ""
      in
      s
    end

let trans grammar =
  List.flatten (List.map (function (l,g) ->
    List.map (function (dl,(dr,(sl,sr))) -> 
      (dl,((dr,(sl,sr)),parse g))) l) grammar);


  
