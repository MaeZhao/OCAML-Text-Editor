open Notty.Infix
open Notty
open Notty_unix

type chr = {
  text:char;
}
(* char *add text stylign in future *)
type document = chr list 

(**for redo and undo*)
type action = Type|Delete
type action_stack = (action*chr)

(* Takes a document representation of characters
   and converts it to an entire string *)
let doc_to_str doc = 
  let str_lst = List.map(fun c-> Char.escaped c.text) doc in 
  List.fold_right (fun acc s-> s^acc) str_lst ""

let save doc = 
  (* print_endline("Please enter name of file"); *)
  (* ANSITerminal.(print_string [white] ">"); *)
  (* let filename = read_line() in  *)
  let _ = ANSITerminal.(print_string [white] 
                          ("input filename:") )  in
  let filename = "textfile" in 
  let _ = print_endline(filename) in 
  let file_channel = open_out (filename^".txt") in 
  let doc_str = doc_to_str doc in 
  let _ = output_string file_channel doc_str in 
  let _ = close_out file_channel in 
  let _ = ANSITerminal.(print_string [white] 
                          (filename^".txt has been saved") ) in doc

let load filename = 
  failwith "unimplemented"

(**todo, a menu funciton *)

(* append character to document *)
let append in_chr doc = 
  (**append a character onto the end of cur line*)
  print_endline(Char.escaped in_chr);
  {text=in_chr}::doc

(* delete a character from document *)
let delete doc = 
  (*delete last inserted character*)
  let _ = print_endline("delete") in 
  match doc with 
  | [] -> doc
  | h::t-> t 


(* print formatting using ansiterminal *)
let ansi_print doc = 
  let str_doc = doc_to_str doc in 
  (* let _ = print_endline(str_doc) in  *)
  let _ = ANSITerminal.(print_string [red] str_doc) in ()

(* helper funciton that converts character to image *)
let chr_to_img ch = 
  I.string A.(fg red ++ bg black) (Char.escaped ch)

(* *unincorporated* [notty_message] converts a string msg to 
   a notty image that would be displayed at the beginning of the document 
   for system messages such as "File has been saved" *)
let notty_message msg = 
  let msg_to_chr_lst = String.to_seq msg in 
  let img_lst = msg_to_chr_lst|>List.of_seq|>List.map(chr_to_img)|>List.rev in 
  let rec loop accImg lst = 
    match lst with 
    | [] -> accImg 
    | h::t -> loop I.(h<|>accImg) t
  in loop (I.string A.(fg red ++ bg black) "") img_lst

(* print formatting using notty *)
let notty_print t doc msg = 
  let len = List.length doc in print_endline(Int.to_string len); 
  let blank_img = I.string A.(fg red ++ bg black) "" in 
  let start_img = notty_message msg in 
  let rec loop accImg accLine lst = 
    match lst with 
    | [] -> I.(accLine<->accImg)
    | h::t -> 
      match h.text with 
      | '\n' -> 
        let nextLine = I.string A.(fg red ++ bg black) "" in 
        let accImg' = I.(accLine<->accImg) in 
        loop accImg' nextLine t 
      | any_char -> 
        let nextLetter = I.string A.(fg red ++ bg black) (Char.escaped h.text) in 
        let accLine' = I.(nextLetter<|>accLine) in 
        loop accImg accLine' t
  in 
  let t = Term.create () in 
  let whole_img = loop start_img blank_img doc in Term.image t whole_img 

(* main loop function to which allowws user to type and delete characters as well
   as save their text into a txt file*)
let main () = 
  print_endline("entered");
  let t = Term.create () in 
  let rec update t notification doc = 
    let noti = 
      (notification ^"| Number of lines:"^
       (string_of_int (List.length(List.filter (fun c -> c.text = '\n') doc)))^
       "| Number of Characters:"^
       (string_of_int (List.length(doc)))^"| CTRL-S to save | ") in
    notty_print t doc noti;
    (* ansi_print doc; *)
    let rec loop t prevDoc = 
      match Term.event t with
      | `Key (`Escape,_)        -> save prevDoc
      | `Key (`Backspace,_) -> delete prevDoc |> update  t ""
      | `Key (`Enter, _) -> append '\n' prevDoc |> update  t ""
      | `Key (`ASCII 'S', (`Ctrl::[])) -> save prevDoc|> update  t "< Saving to textfile.txt >"
      | `Key (`ASCII in_chr , _) -> append in_chr prevDoc |> update  t ""
      | `Resize _              -> loop t prevDoc 
      | _ -> loop t prevDoc
    in
    loop t doc
  in update t "" []




let _ = main ()

(* next steps
   1. allow naming for file
   2. allow opening/loading a file
   3. display system messages
   4. text decoration through adding fields to chr type
    4a. font size changing
    4b. bold, italicize

*)