(* Previous  *)
type object_phrase = string list
(** Navigation Commands *)
type nav_command = 
  | Help 
  | Open of string
  | Delete of string 
  | Create of string
  | Quit

exception Empty

exception Malformed

let parse str = 
  let split_str = String.split_on_char ' ' str |> 
                  List.filter(fun word->word<>"") in
  match split_str with 
  | [] -> raise Empty 
  | h::t-> 
    let action = String.lowercase_ascii h in
    if action = "open" && List.length t = 1 then
      match t with 
      | arg::[] -> Open arg 
      | _ -> raise Malformed
    else if action = "create" && List.length t = 1 then
      match t with 
      | arg::[] -> Create arg 
      | _ -> raise Malformed
    else if action = "delete" && List.length t = 1 then
      match t with 
      | arg::[] -> Delete arg 
      | _ -> raise Malformed
    else if action = "help" && t = [] then Help
    else if action = "quit" && t = [] then Quit
    else raise Malformed

(*______________________*)

(*EDITOR MODULE*)
type text_command = 
  | Insert 
  | Delete
  | Exit (**a repeat..*)

type file_command =
  | Edit
  | Save
  | Undo
  | Redo
  | Exit 

(*FILE MODULE*)

let open_file f = 
  print_endline ("Opening file: "^f);
  failwith "Unimplemented"

let delete_file f = 
  print_endline ("Delete file: "^f);
  failwith "Unimplemented"
let create_file f = 
  print_endline("Creating file: "^f);
  try 
    let _ = Unix.mkdir f 0022 in (** mask 0022 would make the new mask 0644 
                                     (0666-0022=0644) meaning that group and 
                                     others have read (no write or 
                                     execute) permissions*)
    print_endline("Successfully created "^f)
  with _ -> print_endline("Failed to create file "^f)
(* let oc = open_out (f) in () *)
(* failwith "Unimplemented" *)
(*__________________________*)

let get_input () = 
  print_string  "> ";
  String.lowercase_ascii(read_line ()) |> parse

let rec main_loop () = 
  print_endline "Type help for available commands";
  let user_command = get_input () in 
  try 
    match user_command with 
    | Open file -> let _ = open_file file in main_loop()
    | Create file -> let _ =  create_file file in main_loop()
    | Delete file -> let _ =  delete_file file in main_loop()
    | Help -> 
      print_endline "Here are commands you can try:";
      print_endline "[open filename] to open and view the file at filename";
      print_endline "[create filename] to create a file called filename";
      print_endline "[create filename] to create a file called filename";
      main_loop ()
    | Quit -> 
      print_endline "Exiting AnsiEditor";
      exit 1 
  with 
  | Malformed -> main_loop ()
  | _ -> main_loop()



(* let name = "bob" *)
let main () = 
  print_endline "Welcome to AnsiEditor!";
  main_loop ()

(* Execute the game engine. *)

let () = main ()
