open Notty
open Notty_unix

(**Geometry *)
let file = "test.dat"
let square = "\xe2\x96\xaa"

let rec sierp n =
  if n > 1 then
    let ss = sierp (pred n) in I.(ss <-> (ss <|> ss))
  else I.(string A.(fg magenta) square |> hpad 1 0)

let image = sierp 8 |> Notty_unix.output_image

(* TODO: try to pretty pritn this image onto a txt file--this image or 
   any other kind of text--Notty text/visuals are all of Notty.image types-- *)

(**INTERACTIVE VER *)
let img (double, n) =
  let s = sierp n in
  if double then I.(s </> vpad 1 0 s) else s

let rec update t state = 
  Term.image t (img state); loop t state
and loop t (double, n as state) =
  match Term.event t with
  | `Key (`Enter,_)        -> ()
  | `Key (`Arrow `Left,_)  -> update t (double, max 1 (n - 1))
  | `Key (`Arrow `Right,_) -> update t (double, min 8 (n + 1))
  | `Key (`ASCII ' ', _)   -> update t (not double, n)
  | `Resize _              -> update t state
  | _                      -> loop t state

let t = Term.create ()

let _ = update t (false, 1); Term.release t