#use "topfind";;
#require "netstring";;

(* The type of a post on the forum *)
type post =
  {  author  : string;
     date    : string;
     content : string; }

(* Because it's annoying to go DEEPER inception-style in a document,
   we'll define a notion of path in the html doc. It consists of a
   list of instruction to either:
   - Skip the constructor "a"
   - Go DEEPER by visiting the children of the constructor *)

type trace = Skip of string | Rec of string

let rec following trace html =
  match trace, html with
  | []                 , hd :: _ -> hd
  | (Skip hd) :: trace , Nethtml.Element (el , _ , _) :: tl
  | (Rec hd)  :: trace , Nethtml.Element (el , _ , tl) :: _ when hd = el ->
    following trace tl
  | _ -> failwith "*** Error: trace incompatible with the structure of the document"

(* Because sometimes we don't really care about exploration and all we
   really want is to jump all the way to the next constructor with the
   appropriate argument, we can also visit the document DEPTH-first style.*)

let rec find_element elt args = function
  | [] :: tl -> find_element elt args tl
  | (Nethtml.Element (cand , cand_args , children) :: rest) :: tl ->
    if cand = elt && List.for_all (fun a -> List.mem a cand_args) args
    then children
    else find_element elt args (children :: rest :: tl)
  | (Nethtml.Data _ :: rest) :: tl -> find_element elt args (rest :: tl)
  | [] -> raise Not_found

(* Casting operation from Nethtml.Data to its content.
   Use at your own risks. *)

let from_data = function
  | Nethtml.Data s -> s
  | Nethtml.Element (el , _ , _) -> failwith ("*** Error: was expecting data, got " ^ el)

(* Given that the html document is well-positionned, we can extract info from
   the next post available. At the moment we just get the author and the date
   it was posted on. *)

let get_post html =
  let auth_box = find_element "td" [ "class" , "forum-cartouche" ] [ html ]  in
  let author   = from_data (following [ Skip "a" ; Rec "p" ; Rec "a" ]  auth_box) in
  let date     = from_data (following [ Skip "a" ; Skip "p" ; Rec "p" ] auth_box) in
  let msg_box  = find_element "td" [ "class" , "forum-message" ] [ html ] in
  { author ; date ; content = "" }

(* Load the HTML document. *)
let channel = new Netchannels.input_channel (open_in "page1.html")
let html = Nethtml.parse channel
let () = channel#close_in ()

(* Extraction of the information! *)
let () =
  (* Jumping to the table containing the forum posts. *)
  let html =
    find_element "tbody" []
     [ find_element "table" [ "class" , "forum" ] [ Nethtml.decode html ] ] in
  (* Reading the post one by one. *)
  let rec loop posts =
    match posts with
    | [] -> ()
    | _  ->
      let post = get_post posts in
      print_string (post.author ^ "\n" ^ post.date ^ "\n") ;
      (* Why the FUCK do we need three List.tl? 2 should be enough to
         skip the two <tr> constructor (the current post and its footer)
         present before the next post... *)
      loop (List.tl (List.tl (List.tl posts)))
  in loop html
