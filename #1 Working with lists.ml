(*** 99 Problems in OCaml ***)

(** Working with lists **)

(* 1th *)
let rec last list =
  match list with
  | [] -> None
  | [x] -> Some x
  | x :: tl -> last tl;;

(* 2nd *)
let rec last_two list =
  match list with
  | [x ; y] -> Some (x , y)
  | x :: tl -> last_two tl
  | _ -> None;;

(* 3rd *)
let rec at n list =
  match n, list with
  | 1 , x :: _ -> Some x
  | _ , _ :: tl -> at (pred n) tl
  | _ , _ -> None;;

(* 4th *)
let rec length list =
  match list with
  | _ :: tl -> succ (length tl)
  | [] -> 0;;

(* 5th *)
let rec rev list =
  match list with
  | [] -> []
  | x :: tl -> (rev tl) @ [x];;

(* 6th *)
let without_last list =
  match rev list with
  | [] -> []
  | _ :: tl -> rev tl;;

let rec is_palindrome list =
  match list with
  | [] -> true
  | x :: tl -> (Some x = last list) && (is_palindrome (without_last tl));;

let is_palindrome' list =
  let rev_list = rev list in
  let eql = List.map2 (=) list rev_list in
  List.fold_left (&&) true eql;;

(* 7th *)
type 'a node =
  | One of 'a
  | Many of 'a node list;;

let rec flatten nl =
  match nl with
  | (One x) :: tl -> x :: (flatten tl)
  | (Many nl') :: tl -> (flatten nl') @ (flatten tl)
  | _ -> [];;

(* 8th *)
let rec compress list =
  match list with
  | [x] -> [x]
  | x :: tl -> if x = List.hd tl
              then compress tl
              else x :: (compress tl)
  | _ -> [];;

(* 9th *)
let rec pack list =
  match list with
  | [] -> []
  | h :: t ->
     match t with
     | [] -> [[h]]
     | x :: tl -> if h = List.hd (List.hd (pack t))
                 then (h :: (List.hd (pack t))) :: (List.tl (pack t))
                 else [h] :: (pack t);;

(* 10th *)
exception ImplementationError;;
let encode list =
  let f = function
    | [] -> raise ImplementationError
    | x :: t -> (length (x :: t), x)
  in
  List.map f (pack list);;

exception Divide0;;
let rec div n m =
  if m = 0 then raise Divide0
  else n / m;;
  try div 3 0 with
  | Divide0 -> 0;;

(* 11th *)
type 'a rle =
  | One of 'a
  | Many of int * 'a;;

let encode' list =
  let f = function
    | [] -> raise ImplementationError
    | x :: t -> if t = []
               then One x
               else Many (length (x :: t), x)
  in
  List.map f (pack list);;

(* 12th *)
let rec decode list =
  let rec f n x =
    match n with
    | 0 -> []
    | _ -> x :: (f (pred n) x)
  in
  match list with
  | [] -> []
  | h :: t ->
     match h with
     | One y -> y :: (decode t)
     | Many (m , y) -> (f m y) @ (decode t);;

(* 13th *)
let encode'' list =
  let f a init =
    match init with
    | [] -> [One a]
    | h :: t -> match h with
               | One b -> if a = b
                         then Many (2 , a) :: t
                         else (One a) :: init
               | Many (m , b) -> if a = b
                                then Many (m + 1 , b) :: t
                                else (One a) :: init
  in
  List.fold_right f list [];;

(* 14th *)
let rec duplicate list =
  match list with
  | [] -> []
  | h :: t -> (h :: [h]) @ (duplicate t);;

(* 15th *)
let rec replicate list count =
  let rec f x n =
    match n with
    | 0 -> []
    | _ -> x :: (f x (pred n))
  in
  match list with
  | [] -> []
  | h :: t -> (f h count) @ (replicate t count);;

(* 16th *)
let rec drop list count =
  match list , count with
  | [] , _ -> []
  | h :: t , 1 -> t
  | h :: t , _ -> h :: (drop t (pred count));;

(* 17th *)
let rec split list n =
  match list , n with
  | _ , 0 -> ([] , list)
  | [] , _ -> ([] , [])
  | h :: t , _ -> (h :: (fst (split t (pred n))) , (snd (split t (pred n))));;

(* 18th *)
let rec slice list n m =
  if n < m
  then
    match list , n , m with
    | [] , _ , _ -> []
    | _ , 0 , _ -> fst (split list (pred m))
    | h :: t , _ , _ -> slice t (pred n) m
  else [];;

(* 19th *)
let rec rotate list n =
  match list , n with
  | [] , _ -> []
  | _ , 0 -> list
  | h :: t , _ ->
     if n > 0
     then (rotate t (pred n)) @ [h]
     else rotate list (n + (List.length list));;

(* 20th *)
let rec remove_at n list =
  match n , list with
  | 0 , h :: t -> t
  | _ , [] -> []
  | _ , h :: t -> h :: (remove_at (pred n) t);;

(* 21st *)
let rec insert_at item n list =
  match n , list with
  | 0 , _ -> item :: list
  | _ , [] -> []
  | _ , h :: t -> h :: (insert_at item (pred n) t);;

(* 22nd *)
let rec range n m =
  if n = m
  then [n]
  else
    if n > m
    then n :: (range (pred n) m)
    else n :: (range (succ n) m);;

(* 23rd *)
exception ItemNotFound;;
let rec at' n list =
  match n , list with
  | _ , [] -> raise ItemNotFound
  | 0 , h :: t -> h
  | _ , h :: t -> at' (pred n) t;;

let rec rand_select list n =
  match list , n with
  | [] , _ -> []
  | _ , 0 -> []
  | _ , _ ->
     let k =
       Random.int (List.length list)
     in
     (at' k list) :: (rand_select (remove_at k list) (pred n));;

(* 24th *)
let rec lotto_select n m =
  match n with
  | 0 -> []
  | _ -> (Random.int m) :: (lotto_select (pred n) m);;

(* 25th *)
let rec permutation list =
  match list with
  | [] -> []
  | _ ->
     let k =
       Random.int (List.length list)
     in
     (at' k list) :: (permutation (remove_at k list));;

(* 26th *)
let rec list_all_lists = function
  | [] -> [[]]
  | h :: t ->
     let l = list_all_lists t in
     l @ List.map (fun l' -> h :: l') l;;

let rec extract n list =
  let rec filter f = function
    | [] -> []
    | h :: t ->
       if f h
       then h :: (filter f t)
       else filter f t
  in
  filter (fun x -> List.length x = n) (list_all_lists list);;

let rec extract' n list =
  if List.length list = n
  then [list]
  else
    match list with
    | [] -> []
    | x :: tl -> (List.map (fun l -> x :: l) @@ extract' (pred n) tl) @ extract' n tl;;

(* 27th *)
let rec group_n n list =
  if n = 0 then [([] , list)]
  else
    match list with
    | [] -> []
    | h :: t ->
       let rest1 = group_n (pred n) t in
       let rest2 = group_n n t in
       List.map (fun (fst , snd) -> ((h :: fst) , snd)) rest1
       @ List.map (fun (fst , snd) -> (fst , h :: snd)) rest2;;

let rec group list = function
  | [] -> [[]]
  | h :: t ->
     let l = group_n h list in
     List.map (fun (fst , snd) ->
               List.map (fun l -> fst :: l) (group snd t)) l
  |> List.concat;;

(* 28th *)
