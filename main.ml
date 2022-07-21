(* 01 *)
let rec last = function
    | [] -> None
    | [x] -> Some x
    | _ :: rest -> last rest;;

(* 02 *)
let rec last_two = function
    | [] -> None
    | [x; y] -> Some (x, y)
    | [_] -> None
    | _ :: rest -> last_two rest;;

(* 03 *)
let rec at l x = match l with
    | [] -> None
    | hd :: tail when x = 0 -> Some hd
    | hd :: tail -> at tail (x - 1)

