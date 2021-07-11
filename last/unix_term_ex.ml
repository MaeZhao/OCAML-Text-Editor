open Notty
open Notty_unix

(* ocamlbuild -pkg notty -pkg notty.unix basics_Term_simple_terminal.native
 * or
 * ocamlfind ocamlc -o basics_simple_terminal -package notty,notty.unix -linkpkg -g basics_Term_simple_terminal.ml*)

let  rec main_loop t acc =
  if (acc = ' ') then
    let img = I.(string A.(bg lightred ++ fg black) "This is a simple example") in
    Term.image t img;
    match Term.event t with
    | `Key (`Escape, [])-> ()
    |`Key (`ASCII d, [])-> (main_loop t (d + " "))
    | _ -> (main_loop t ' ')
  else 
    let img = I.(string A.(bg lightred ++ fg black) acc) in
    Term.image t img;
    match Term.event t with
    | `Key (`Escape, [])-> ()
    |`Key (`ASCII d, [])-> (main_loop t d)
    | _ -> (main_loop t ' ')

(* let rec smloop t acc =
   if acc then
    let img = I.(string A.(bg lightred ++ fg black) "This is a simple example") in
    Term.image t img;
   else
    let img = I.(string A.(bg lightred ++ fg black) "This is a simple example!") in
    Term.image t img;
    (* in
       let _ = 
       match Term.event t with
       | `Key (`Escape, [])-> ()
       |`Key (`ASCII d, [])-> (smloop t false)
       | _ -> smloop t (acc && true) *)
   in smloop t acc *)

let _ =
  let t = Term.create () in main_loop t ' '