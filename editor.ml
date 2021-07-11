open Notty.Infix
open Notty
open Notty_unix
open Printf

(* type of characters in our text editor *)
type chr = {
  text:char;
}

(* [document] is a document of our text editor of type [chr] *)
type document =  chr list 


(**An action represents the various kinds of actions we can perform in the 
   editor*)
type action = 
  | Type of chr
  | Delete of chr
  | Undo of action

type action_stack = action list

(* [doc_to_str doc] converts a [chr list] doc to a string *)
let doc_to_str doc = 
  let str_lst = List.map(fun c-> Char.escaped c.text) doc in 
  List.fold_right (fun acc s-> s^acc) str_lst "" 

(** [doc_to_file doc output] converts a [chr list] doc into a text output to
    output buffer [output]. *)
let doc_to_file doc output =
  let char_lst = List.map(fun c-> c.text) doc  in
  let f c = output_char output c in 
  List.iter f (List.rev char_lst)


(** [save doc name] saves a doc to a file called "n.txt" where
    n is [name] *)
let save doc name= 
  (* print_endline("Please enter name of file"); *)
  (* ANSITerminal.(print_string [white] ">"); *)
  (* let filename = read_line() in  *)
  let filename = doc_to_str name in
  let _ = print_endline(filename) in 
  let file_channel = open_out (filename^".txt") in  
  (* let doc_str = doc_to_str doc in 
     let _ = output_string file_channel doc_str in  *)
  let _ = doc_to_file doc file_channel in
  let _ = close_out file_channel in 
  let _ = ANSITerminal.(print_string [white] 
                          (filename^".txt has been saved") ) in (doc , name)

(** [append in_chr doc] adds the inputted character to the document *)
let append in_chr doc = 
  (**append a character onto the end of cur line*)
  print_endline(Char.escaped in_chr);
  {text=in_chr}::doc

(** [load filename] loads a text file named [filename] and converts it to a chr
    list *)
let load name =
  let filename = doc_to_str name in
  if(Sys.is_directory(filename)) then(
    let file_channel = open_in (filename^".txt") in 
    let char_stream = Stream.of_channel(file_channel) in
    let new_doc = [] in
    let rec loop char_stream new_doc = 
      if (Stream.peek(char_stream)= None) then
        new_doc 
      else
        (let c = Stream.next(char_stream) in
         loop char_stream (append c new_doc))
    in ((loop char_stream new_doc),name))
  else (
    let new_doc = [] in
    save new_doc name
  )

(** [delete doc] deletes the last character from [doc] *)
let delete doc = 
  (*delete last inserted character*)
  let _ = print_endline("delete") in 
  match doc with 
  | [] -> doc
  | h::t-> t 


(*MS2 undo and redo*)
(* let undo curStack curDoc : document= 
   match curStack with 
   | [] -> curDoc
   | h::t -> 
    match h with 
    | Type some_char -> (match curDoc with 
        | [] -> [] 
        | last_char::rest_of_doc -> rest_of_doc)
    | Delete some_char -> append some_char.text curDoc

   let redo curStack curDoc : document = 
   match curStack with 
   | [] -> curDoc
   | h::t -> 
    match h with 
    | Type some_char -> (match curDoc with 
        | [] -> [] 
        | last_char::rest_of_doc -> rest_of_doc)
    | Delete some_char -> append some_char.text curDoc *)


(* print formatting using ansiterminal *)
let ansi_print doc = 
  let str_doc = doc_to_str doc in 
  (* let _ = print_endline(str_doc) in  *)
  let _ = ANSITerminal.(print_string [red] str_doc) in ()

(* helper function that converts character to image *)
let chr_to_img ch = 
  I.string A.(fg black ++ bg white) (Char.escaped ch)

(** [notty_message] converts a string msg to 
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


(** [notty_print_doc doc] converts the chr list [doc] into a Notty image *)
let notty_print_doc doc= 
  let len = List.length doc in print_endline(Int.to_string len); 
  let blank_img = I.string A.(fg black  ++ bg yellow) "" in 
  let rec loop accImg accLine lst = 
    match lst with 
    | [] -> I.(accLine  <-> accImg)
    | h::t -> 
      match h.text with  
      | '\n' -> 
        (** current line *)
        let nextLine = I.string A.(fg black  ++ bg yellow) "" in 
        let accImg' = I.(accLine  <-> accImg) in 
        loop accImg' nextLine   t 
      | any_char -> let nextLetter = 
                      if ((List.length t) = ((List.length doc)-1)) then 
                        ((** current letter *)
                          I.string A.(fg black  ++ bg yellow) (Char.escaped h.text)) 
                      else
                        (I.string A.(fg white ++ bg blue) (Char.escaped h.text)) in 
        let accLine' = I.(nextLetter<|>accLine) in 
        loop accImg accLine' t
  in 
  loop  blank_img blank_img doc

(* [notty_print_notification doc notification input] takes a user input [input]
     and a string [notification] and returns a Notty image displaying the 
     number of lines and characters in the document and user input for filenames
     in functionalitites like opening and saving files. *)
let notty_print_notification doc notification input= 
  let line_count = if (List.length(doc) = 0) then 0 
    else (1 + List.length(List.filter (fun c -> c.text = '\n') doc)) in
  let msg = 
    ("Number of lines:"^
     (string_of_int (line_count))^
     "| Number of Characters:"^
     (string_of_int (List.length(doc)- 
                     line_count)) ^ "| " ^notification ) in
  let blank_img = I.string A.(fg red ++ bg black) "" in 
  let start_img = notty_message msg in 
  let rec loop accImg accLine lst = 
    match lst with 
    | [] -> I.(accImg   <|> accLine)
    | h::t ->  
      match h.text with
      | any_char -> 
        let nextLetter = I.string A.(fg red ++ bg black) (Char.escaped h.text) in 
        let accLine' = I.(nextLetter<|>accLine) in 
        loop accImg accLine' t
  in 
  loop start_img blank_img input

(* [notty_print doc notification input] takes the images produced by 
    [notty_print_notification doc notification input] and [notty_print_doc doc]
    and collates them into one Notty image to be displayed on the terminal. *)
let notty_print doc notification input= 
  let t = Term.create () in
  let whole_img = I.((notty_print_notification doc notification input)
                     <->
                     (notty_print_doc doc)) in
  (Term.image t whole_img, t)



(** [update notification doc actionStack undoStack filename saved] updates the 
    state of the editor with every keystroke.
    [actionStack] is the current stack of recorded keystrokes of type action list
    [undoStack] is the previous list of recorded keystrokes of type action list
    [saved] is a boolean value the represents whether or not our [doc] has been 
    saved to a specific file*)
let rec update notification doc actionStack undoStack filename saved= 
  let terminal = notty_print doc notification [] in

  (* processor takes in Key strokes from user and list and returns a resulting action list *)
  let rec processor t prevDoc actionStack undoStack filename saved= 
    match Term.event t with
    | `Key (`Escape,_) -> save prevDoc filename
    | `Key (`Backspace,_) -> 
      let new_doc = delete prevDoc in 
      let new_stack = 
        (match prevDoc with 
         | [] -> actionStack
         | h::t -> (Delete h)::actionStack
        ) in 
      update  "" new_doc new_stack [] filename saved
    | `Key (`Enter, _) -> 
      let new_doc = append '\n' prevDoc in
      let new_stack = (Type {text='\n'})::actionStack in 
      update  "" new_doc new_stack [] filename saved
    | `Key (`ASCII 'S', (`Ctrl::[])) -> 
      (* Seperate save processor for saving documents *)
      if (saved = true) then (let new_doc = fst (save doc filename) in 
                              update "< Saved >" new_doc actionStack undoStack filename true) 
      else (
        let rec update_save_notification input doc   =
          let notification = "Enter Text File Name to Save: " in
          let t = Term.create() in 
          let img = notty_print_notification doc notification input
          in Term.image t img;
          let rec notification_processor t prevInput   =
            match Term.event t with
            | `Key (`Escape,_) -> save doc prevInput
            | `Key (`Backspace,_) -> 
              let new_input = delete prevInput in 
              update_save_notification  new_input  doc
            | `Key (`Enter, _) -> save doc prevInput
            | `Key (`ASCII 'S', (`Ctrl::[])) -> save doc prevInput
            | `Key (`ASCII in_chr , _) -> 
              let new_input = append in_chr prevInput in 
              update_save_notification new_input doc
            (* match prevDoc with  *)
            (* | [] -> update  *)
            | `Resize _              -> notification_processor t prevInput 
            | _ -> notification_processor t prevInput ; in
          notification_processor t input
        in
        let saved = update_save_notification [] doc in
        let new_doc = fst(saved) in
        let prevInput = snd(saved) in 
        update  "< Saved >" new_doc actionStack undoStack prevInput true)
    | `Key (`ASCII 'O', (`Ctrl::[])) -> 
      (* Seperate open processor for loading documents *) (
        let rec upload_new_file input doc   =
          let notification = "Enter Text File Name to Open: " in
          let t = Term.create() in 
          let img = notty_print_notification doc notification input
          in Term.image t img;
          let rec notification_processor t prevInput   =
            match Term.event t with
            | `Key (`Escape,_) -> load prevInput
            | `Key (`Backspace,_) -> 
              let new_input = delete prevInput in 
              upload_new_file  new_input  doc
            | `Key (`Enter, _) -> load prevInput
            | `Key (`ASCII 'S', (`Ctrl::[])) -> load prevInput
            | `Key (`ASCII in_chr , _) -> 
              let new_input = append in_chr prevInput in 
              upload_new_file new_input doc
            (* match prevDoc with  *)
            (* | [] -> update  *)
            | `Resize _              -> notification_processor t prevInput 
            | _ -> notification_processor t prevInput ; in
          notification_processor t input
        in
        let saved = upload_new_file [] doc in
        let new_doc = fst(saved) in
        let prevInput = snd(saved) in 
        update "< Saved >" new_doc [] [] prevInput true)
    | `Key (`ASCII 'Z', (`Ctrl::[])) -> 
      (
        match actionStack with 
        | [] -> update  "" prevDoc actionStack undoStack filename saved
        | lastAction::rest_of_actions -> (
            match lastAction with 
            | Type someChar ->  
              let new_doc = match prevDoc with | [] -> [] | lastLetter::rest_of_doc -> rest_of_doc in 
              update "" new_doc rest_of_actions (Undo lastAction::undoStack) filename saved
            | Delete someChar -> 
              let new_doc = someChar::prevDoc in 
              update "" new_doc rest_of_actions (Undo lastAction::undoStack) filename saved
            | _ -> failwith "malformed action type in actionstack"
          )
      )
    | `Key (`ASCII 'Y', (`Ctrl::[])) -> 
      (
        match undoStack with 
        | [] -> update "" prevDoc actionStack undoStack filename saved
        | lastUndo::rest_of_undos -> (
            match lastUndo with 
            | Undo Type someChar ->  (*want to type*)
              let new_doc = someChar::prevDoc in 
              let new_actionStack = (Type someChar)::actionStack in 
              update "" new_doc new_actionStack rest_of_undos filename saved
            | Undo Delete someChar -> (**seems like what charaacter is deleted or inserted doesn't matter for undo, just redoing whatever was done)*)
              (match prevDoc with 
               | [] -> update "" prevDoc actionStack undoStack filename saved
               | last_letter::rest_of_doc -> 
                 let new_actionStack = (Delete someChar)::actionStack in 
                 update "" rest_of_doc new_actionStack rest_of_undos filename saved
              ) 
            (* let new_doc = someChar::prevDoc in 
               update "" new_doc rest_of_actions (Undo lastAction::undoStack) *)
            | _ -> failwith "malformed action type in actionstack"
          )
      )
    | `Key (`ASCII in_chr , _) -> 
      let new_doc = append in_chr prevDoc in 
      let new_stack = (Type {text=in_chr})::actionStack in 
      update "" new_doc new_stack [] filename saved

    (* match prevDoc with  *)
    (* | [] -> update  *)
    | `Resize _              -> processor t prevDoc actionStack undoStack filename saved
    | _ -> processor t prevDoc actionStack undoStack filename saved; in
  processor (snd terminal) doc actionStack undoStack filename saved

(* [main] loop function to which allows user to type and delete characters as well
   as save their text into a txt file*)
let main () = 
  print_endline("entered");
  let t = Term.create () in
  (* let _ = show_cursor true; Term.cursor t (Some (0,1)); in  *)
  (* ansi_print doc; *)
  update "" [] [] [] [] false

let _ = main ()
