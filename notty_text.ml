(* Copyright (c) 2016-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(**
 * Demonstrates input parsing.
*)
open Notty.Infix
open Notty
open Notty_unix

let chr_to_screen chr curLine = 
  let s = I.string A.(fg red ++ bg black) (Char.escaped chr )
  in 
  I.(curLine<|>s)
(* I.(prevImg<|>s) *)

let rec update_type t prevImg curLine = 
  print_endline("entered");
  Term.image t (prevImg <-> curLine) ; (*This prints out a given IMAGE to a TERM?*)
  let rec loop t prevImg = 
    match Term.event t with
    | `Key (`Escape,_)        -> ()
    | `Key (`Enter, _) -> 
      let new_line = I.string A.(fg red ++ bg black) ">" in 
      let new_img = I.(prevImg<->curLine) in 
      update_type t new_img new_line
    | `Key (`ASCII chr , _)   ->  
      let new_line = (chr_to_screen chr curLine) in 
      update_type t prevImg new_line
    | _                      -> loop t prevImg
  in loop t prevImg 

let t = Term.create ()
let start_img = I.string A.(fg red ++ bg black) "Beginning of Document"
let start_line = I.string A.(fg red ++ bg black) "> "
let _ = update_type t start_img start_line
(* let _ = update t (false, 1); Term.release t *)