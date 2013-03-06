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

let reify_color = function
  | "f4ac00" -> "yellow"
  | "f77400" -> "orange"
  | "ed6161" -> "fuschia"
  | "d50000" -> "red"
  | "maroon" -> "maroon"
  | "5e432d" -> "brown"
  | "purple" -> "purple"
  | "navy" -> "navy"
  | "2b2be4" -> "blue"
  | "5577bc" -> "lightblue"
  | "teal" -> "teal"
  | "219c5a" -> "lightgreen"
  | "006f00" -> "green"
  | "olive" -> "olive"
  | "gray" -> "gray"
  | "5a5a5a" -> "darkgray"
  | _ -> failwith "*** Error: unexpected font color"

let reify_div = function
  | "quote" -> "quote"
  | "limg" -> "fleft"
  | "rimg" -> "fright"
  | div -> failwith "*** Error: unexpected div class (" ^ div ^ ")"

let in_brackets str = "[" ^ str ^ "]" , "[/" ^ str ^ "]"

let reify_bbcode el args =
  match el with
  | "br" -> "\n" , ""
  | "b" | "i" | "u" | "strike" | "pre" | "center" -> in_brackets el
  | "a" -> "[url=" ^ List.assoc "href" args ^ "]" , "[/url]"
  | "p" -> in_brackets (List.assoc "align" args)
  | "div" ->
    let div = List.assoc "class" args in
    if div = "left" || div = "right" then in_brackets div else "" , ""
  | "font" -> in_brackets (reify_color (List.assoc "color" args))
  | "img" ->
      begin try List.assoc "alt" args , ""
      with Not_found -> "[img]" ^ List.assoc "src" args ^ "[/img]" , "" end
  | _  -> "" , ""

let rec revert_bbcode acc = function
  | Nethtml.Element ("div" , [] , children) :: tl ->
    (* DEAL WITH SPOILERS *) revert_bbcode (revert_bbcode acc children) tl
  | Nethtml.Element (el , args , children) :: tl ->
     let start , finish = reify_bbcode el args in
     revert_bbcode (finish :: revert_bbcode (start :: acc) children) tl
  | Nethtml.Data s :: tl -> revert_bbcode (s :: acc) tl
  | [] -> acc


let get_post html =
  let auth_box = find_element "td" [ "class" , "forum-cartouche" ] [ html ]  in
  let author   = from_data (following [ Skip "a" ; Rec "p" ; Rec "a" ]  auth_box) in
  let date     = from_data (following [ Skip "a" ; Skip "p" ; Rec "p" ] auth_box) in
  let msg_box  =
    find_element "div" []
    [ find_element "td" [ "class" , "forum-message" ] [ html ] ] in
  let content = String.concat "" (List.rev (revert_bbcode [] msg_box)) in
  { author ; date ; content }

(* Load the HTML document. *)
let channel = new Netchannels.input_channel (open_in "page2.html")
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
      print_string
         ( "[b]Auteur originel :[/b] "^ post.author ^ "\n[b]Date de publication :[/b] "
         ^ post.date ^ "\n\n" ^ post.content ^ "\n\n\n\n") ;
      (* Why the FUCK do we need three List.tl? 2 should be enough to
         skip the two <tr> constructor (the current post and its footer)
         present before the next post... *)
      loop (List.tl (List.tl (List.tl posts)))
  in loop html
