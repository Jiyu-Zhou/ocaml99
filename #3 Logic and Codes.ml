(*** 99 Problems in Ocaml ***)

(** Logic and Codes **)

#use "#2 Arithmetic.ml";;

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr;;

(* 40th *)
let rec eval2 a b expr =
  match expr with
  | Var v ->
    let (var_a , val_a) = a in
    let (var_b , val_b) = b in
    if v = var_a then val_a
    else if v = var_b then val_b
    else assert false
  | Not x -> not (eval2 a b x)
  | And(x1 , x2) -> eval2 a b x1 && eval2 a b x2
  | Or(x1 , x2) -> eval2 a b x1 || eval2 a b x2

let table2 a b expr =
  let truth_table = [(true, true); (true, false); (false, false); (false, true)] in
  let f (val_a, val_b) = (val_a, val_b, eval2 (a, val_a) (b, val_b) expr) in
  List.map f truth_table

let test40 = table2 "a" "b" (And(Var "a", Or(Var "a", Var "b"))) =
             [(true, true, true); (true, false, true);
              (false, false, false); (false, true, false); ]

(* 41st *)
exception ItemNotFound;;
let rec find list str =
  if list = [] then raise ItemNotFound
  else if fst (List.hd list) = str then snd (List.hd list)
  else find (List.tl list) str;;

let rec eval list expr =
  match expr with
  | Var x -> find list x
  | Not x -> not (eval list x)
  | And(x1 , x2) -> eval list x1 && eval list x2
  | Or(x1 , x2) -> eval list x1 || eval list x2;;

let rec pattern list =
  match list with
  | [] -> []
  | [x] -> [[(x , true)];[(x , false)]]
  | h :: t ->
    let sub = pattern t in
    (List.map (fun l -> (h , true) :: l) sub)
    @ (List.map (fun l -> (h , false) :: l) sub);;

let table list expr =
  List.map (fun x -> (x , eval x expr)) (pattern list);;

(* 42nd *)
let rec binary_gen n =
  match n with
  | 0 -> []
  | 1 -> [[1];[0]]
  | _ -> let sub = binary_gen (n - 1) in
    (List.map (fun l -> 1 :: l) sub)
    @ (List.map (fun l -> 0 :: l) sub);;

let rec lsc l =
  match l with
  | [] -> ""
  | h :: t -> if h = 1 then "1" ^ (lsc t)
    else "0" ^ (lsc t);;

let gray n =
  List.rev (List.map lsc (binary_gen n));;

(* 43rd *)
let rec is_sort list =
  match list with
  | [] -> true
  | [x] -> true
  | h :: t -> (h >= (List.hd t)) && (is_sort t);;

let is_pair_list_sort list =
  let aux = (fun (x , y) -> y) in
  if is_sort (List.map aux list) then true else false;;

let sort_pair_list list =
  let rec aux l =
    match l with
    | [] -> []
    | [x] -> l
    | h :: t ->
      let a = List.hd t in
      let y = List.tl t in
      if snd h < snd a then a :: (aux (h :: y))
      else h :: (aux t)
  in
  if is_pair_list_sort (aux list) then aux list else List.rev (aux (aux list));;

type 'a tree =
  | Nl
  | Lf of 'a * int
  | Br of 'a tree * 'a tree * int;;

let node tr1 tr2 =
  match tr1, tr2 with
  | Lf (_ , n1), Lf (_ , n2) -> Br (tr1, tr2, (n1 + n2))
  | Lf (_ , n1), Br (_ , _ , n2) -> Br (tr1, tr2, (n1 + n2))
  | Br (_ , _ , n1), Lf (_ , n2) -> Br (tr1, tr2, (n1 + n2))
  | Br (_ , _ , n1), Br (_ , _ , n2) -> Br (tr1, tr2, (n1 + n2));;

let huffman_tree list =
  let rec aux list tr =
    match list, tr with
    | [] , _ -> tr
    | (c , f) :: t , Nl -> aux t (Lf c f)
    | (c , f) :: t , Lf c' f' ->
