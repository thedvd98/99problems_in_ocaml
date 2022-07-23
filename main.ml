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
    | hd :: tail -> at tail (x - 1);;

(* 04 *)
let rec length l = match l with
    | [] -> 0
    | hd :: tail -> 1 + (length tail);;

(* 04 Tail recursive version *)
let length_tail l =
    let rec iter l c = match l with
        | [] -> c
        | hd :: tail -> (iter tail (c + 1))
    in
        iter l 0;;

(* 05 *)
let rec rev l = match l with
    | [] -> []
    | hd :: tail -> (rev tail) @ [hd];;

(* 06 *)
let is_palindrome l =
    (rev l) = l;;

(* 07 *)
type 'a node =
    | One of 'a
    | Many of 'a node list;;

let rec flatten = function
    | [] -> []
    | One x :: tail -> x :: (flatten tail)
    | Many x :: tail -> (flatten x) @ (flatten tail)
;;

(* 08 *)
(* my version *)
let rec compress = function
    | [] -> []
    | [x] -> [x]
    | hd :: tail ->
            match tail with
                | [] -> []
                | next_hd :: next_tail ->
                        if hd = next_hd then
                            (compress (hd::next_tail))
                        else
                            hd::(compress (next_hd::next_tail))
;;
(* solution version *)
let rec compress_easy = function
    | a :: (b :: _ as tail) -> if a = b then compress tail else a :: compress tail
    | smaller -> smaller;;

(* 09 *)
let rec tail_from l n = match l with
    | [] -> []
    | x when n <= 0 -> x
    | hd :: tail -> tail_from tail (n-1)
;;
let rec pack l =
    let rec iter li = match li with
    | a :: (b :: _ as tail) -> if a = b then a :: (iter tail) else [a]
    | [] -> []
    | x -> x
    in
    match l with
        | [] -> []
        | _ :: tail -> let part = (iter l)
    in
        part :: (pack (tail_from l (List.length part)))
;;
