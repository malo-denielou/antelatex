(********************************************************************)
(* Antelatex - Main                                                 *)
(********************************************************************)
(* $Time-stamp: <Malo - 2012>$ *)

open Common

let version=
  let v = Version.version in
  Printf.sprintf 
    "Antelatex version: %s" v

let debug = Common.debug "Main"
let fulldebug = Common.fulldebug "Main"

let grammar_file = ref None
let latex_files : (string list) ref = ref [] 

let msg_usage = 
  "Usage: antelatex [-v] -g GRAM FILE ...\n"^
    "Processes the FILE(s) according to the grammar file GRAM."
 
let speclist = Arg.align
  [ ("-v", 
     Arg.Unit switch_debug_on,
     ": verbose mode");
     ("-vv", 
     Arg.Unit switch_fulldebug_on,
     ": verbose mode with additional output");
     ("--version",
      Arg.Unit (function () -> print_string version; exit 0),
      ": outputs the version number and exits");
    ("-g",
     Arg.String (fun s -> grammar_file := Some s),
     ": sets the ")
  ]
 
let main () =
  Arg.parse
    speclist
    (fun x -> latex_files := (!latex_files)@[x])
    msg_usage;
  let () = debug "Starting antelatex" in
  let transform = match !grammar_file with
    | None -> (debug "No grammar file specified" ; [])
    | Some g -> Trans.trans (Grammar.grammar g) in
  if !latex_files = [] then
    debug "No files to process";
  List.iter
    (fun source_file ->
      debug ("Processing file "^source_file);
      if Filename.check_suffix source_file "tex"
      then 
        failwith 
          "The files to be processed should not have .tex as extension.";
      let dest_file = (Filename.chop_extension source_file)^".tex" in
      let ch = Io.create_chan source_file dest_file in
      (try (Preproc.apply transform ch)
       with
           Common.Parse_error (err,info) -> 
             prerr_string 
               (Printf.sprintf 
                  "Error in %s within the block starting from %s:\n%s\n" 
                  source_file (info_to_string info) err);
             exit 1
      );
      flush_all ();
      Io.close_chan ch
    ) !latex_files;
  let () = debug "Closing antelatex" in
  ()
  
    
let () = 
  main ()
  



