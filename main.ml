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
(* my version *)
let rec tail_from l n = match l with
    | [] -> []
    | x when n <= 0 -> x
    | hd :: tail -> tail_from tail (n-1);;

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
        part :: (pack (tail_from l (List.length part)));;
(* solution version *)
let pack_best l =
    let rec aux current acc = function
        | [] -> []
        | [x] -> (x::current)::acc
        | a :: (b :: _ as tail) ->
                if a = b then
                    aux (a::current) acc tail
    else
        aux [] ((a::current)::acc) tail
        in
    List.rev (aux [] [] l);;


(* 10 *)
(* Run length encoding *)
let encode li = 
    let rec iter l last acc = match l with
    | [] -> []
    | [x] -> (last :: acc)
    | a :: (b :: _ as tail) ->
            if a = b then
                let (n, _) = last in
                iter tail (n+1, a) acc
                else
                    iter tail (1, b) (last :: acc)
                in
    List.rev(iter li (1, "DIO") []);;
(* Version using pack *)
let encode_short li =
    List.map (fun l -> (List.length l, List.hd l)) (pack li);;

(* 11 *)
type 'a rle =
    | One of 'a
  | Many of int * 'a

let encode_short_mod li =
    List.map (fun l ->
        let len = List.length l in
        if len = 1 then One (List.hd l) else (Many (len, List.hd l)))
    (pack li);;

(* 12 *)
(* my solution *)
let rec decode li =
    let rec make_list a count = match count with
        | 0 -> []
        | _ -> a::(make_list a (count - 1))
        in
    let decode_tuple = function
        | One a -> [a]
        | Many (count, a) -> (make_list a count)
    in
    match li with
        | [] -> []
        | hd :: tail -> (decode_tuple hd) @ (decode tail);;

(* solution *)
let rec decode_best li =
    let rec make_list a count = match count with
        | 0 -> []
        | _ -> a::(make_list a (count - 1))
    in
    match li with
        | [] -> []
        | One a :: tail -> [a] @ (decode_best tail)
        | Many (count, a) :: tail -> (make_list a count) @ (decode tail);;

(* 13 *)
(* Run-length encoding of a list (direct solution) *)

let encode_rle li = 
    let make_many a count =
        (Many (count, a))
    in
    let make_one a =
        (One a)
    in
    let rec iter l count = match l with
        | [] -> []
        | a :: (b :: _ as tail) ->
                if a = b then
                    iter tail (count + 1)
        else
            if count > 1 then
                (make_many a count) :: (iter tail 1)
            else
                (make_one a) :: (iter tail 1)
        | [x] ->
                if count > 1 then
                    [(make_many x count)]
                else
                    [(make_one x)]

    in
    (iter li 1)
;;

(* 14 *)
(* Duplicate the elements of a list *)

let rec duplicate li = match li with
    | [] -> []
    | [x] -> [x; x]
    | hd :: tail -> hd::hd::(duplicate tail);;


(* 15 *)
(* Replicate the elements of a list a given number of times *)

let replicate li n =
    let rec iter li n count = match li with
        | [] -> []
        | [x] when count > 1 -> x::(iter li n (count - 1))
        | [x] when count <= 1 -> [x]
        | hd :: tail when count > 1 ->
                hd::(iter li n (count - 1))
        | hd :: tail when count <= 1 ->
                hd::(iter tail n n)
        | _ -> []
    in
    iter li n n;;

(* 16 *)
(* Drop every N'th element from a list *)
let drop li n =
    let rec iter li count = match li with
      | [] -> []
      | hd :: tail ->
        if count = 0 then
          (iter tail (n-1))
        else
          hd :: (iter tail (count-1))
    in
    iter li (n-1);;

(* 17 *)
(* Split a list into two parts *)
let rec split li c = match li with
  | [] -> ([],[])
  | hd :: t ->
    if c > 0 then
      let (a,b) = (split t (c-1)) in
      (hd::a, b)
    else
      let (a,b) = (split t 0) in
      (a, hd::b);;

let split_tail_recursive li n =
  let rec iter li c acc = match li with
    | [] -> acc
    | hd :: t -> let (a,b) = acc in
      if c > 0 then (iter t (c-1) (a@[hd], b))
                    else (iter t 0 (a, b@[hd]))
  in
  iter li n ([], []);;

(* 18 *)
(* Extract a slice from a list *)
let rec slice li st en = match li with
  | [] -> []
  | hd::t ->
    if st < 1 && en >= 0 then
      hd::(slice t (st-1) (en-1))
    else
      (slice t (st-1) (en-1));;

(* 19 *)
(* rotate *)
let rec rotate li n =
  let length = List.length li in
  let a = if length = 0 then 0 else n mod length in
  if a <= 0 then
    li
  else
    let (first_part, second_part) = split_tail_recursive li a in
    second_part @ first_part;;

(* 20 *)
(* remove K'th element *)
let remove_at k li =
  let rec iter c result = function
    | [] -> result
    | _::t when c = k -> (iter (c + 1) result t)
    | hd::t -> (iter (c + 1) (hd::result) t)
  in
  List.rev (iter 0 [] li);;

(* easier *)
let rec remove_at_ k = function
  | [] -> []
  | hd::t -> if k = 0 then (remove_at_ (k-1) t) else hd::(remove_at_ (k-1) t);;
