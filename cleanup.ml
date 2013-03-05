



(*
let first_trim =
  Str.regexp "<html>(.\\|\n)*<table class=\"forum\"><colgroup width=\"165\"></colgroup><colgroup></colgroup><tbody>\\((.\\|\n)*\\)</tbody></table><a name=\"end\"(.\\|\n)*</html>"
*)

let first_trim =
  Str.regexp "<html><pre>\\((.\\|\n)*\\)</pre>(.\\|\\n)*</html>"

let trim file =
  print_string (Str.replace_first first_trim "" file)


(**********************************************
****************** Executable *****************
**********************************************)

let usage () = print_string "./cleanup FILE_TO_CLEAN\n"


(* Taken from Rosetta stone *)
let load_file filepath =
  let ic = open_in filepath in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  s

let run () =
  match List.tl (Array.to_list Sys.argv) with
  | filepath :: [] ->
    begin
    if Sys.file_exists filepath
    then trim (load_file filepath)
    else Printf.printf "*** Error: file %s does not exist!\n" filepath
    end
  | _ -> usage ()

let _ = run ()
