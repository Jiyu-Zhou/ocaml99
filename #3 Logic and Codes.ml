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
exception ItemNotFound

let rec find str assoc =
  List.assoc str assoc

let rec eval list = function
  | Var x -> find x list
  | Not e -> not (eval list e)
  | And(e1 , e2) -> eval list e1 && eval list e2
  | Or(e1 , e2) -> eval list e1 || eval list e2

let rec pattern = function
  | [] -> []
  | [x] -> [[(x , true)]; [(x , false)]]
  | h :: t ->
    let sub = pattern t in
    let cons x l = x :: l in
    (List.map (cons (h , true)) sub)
    @ (List.map (cons (h , false)) sub)

let table vars expr =
  let f x = (x , eval x expr) in
  List.map f @@ pattern vars

let test41_1 =
  table ["a"; "b"] (And(Var "a", Or(Var "a", Var "b"))) =
  [([("a", true); ("b", true)], true); ([("a", true); ("b", false)], true);
   ([("a", false); ("b", true)], false); ([("a", false); ("b", false)], false)]

let test41_2 =
  let a = Var "a" and b = Var "b" and c = Var "c" in
  table ["a"; "b"; "c"] (Or(And(a, Or(b,c)), Or(And(a,b), And(a,c)))) =
  [([("a", true); ("b", true); ("c", true)], true);
   ([("a", true); ("b", true); ("c", false)], true);
   ([("a", true); ("b", false); ("c", true)], true);
   ([("a", true); ("b", false); ("c", false)], false);
   ([("a", false); ("b", true); ("c", true)], false);
   ([("a", false); ("b", true); ("c", false)], false);
   ([("a", false); ("b", false); ("c", true)], false);
   ([("a", false); ("b", false); ("c", false)], false)]

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
  | Lf of 'a * int
  | Br of ('a tree * 'a tree) * int;;

let node tr1 tr2 =
  match tr1, tr2 with
  | Lf (_ , n1), Lf (_ , n2) -> Br ((tr1, tr2), (n1 + n2))
  | Lf (_ , n1), Br ((_ , _) , n2) -> Br ((tr1, tr2), (n1 + n2))
  | Br ((_ , _) , n1), Lf (_ , n2) -> Br ((tr1, tr2), (n1 + n2))
  | Br ((_ , _) , n1), Br ((_ , _) , n2) -> Br ((tr1, tr2), (n1 + n2));;

let rec sort list =
  let get_int x =
    match x with
    | Lf (_ , i) -> i
    | Br ((_ , _) , i) -> i
  in
  let rec more x list =
    match list with
    | [] -> []
    | h :: t -> if get_int h >= x
      then h :: (more x t)
      else more x t
  in
  let rec less x list =
    match list with
    | [] -> []
    | h :: t -> if get_int h < x
      then h :: (less x t)
      else less x t
  in
  match list with
  | [] -> []
  | h :: _ -> let i = get_int h in
    (sort (less i list))
    @ (h :: (sort (more i list)));;

let huffman_tree list =
  let rec aux list tr =
    match list, tr with
    | [] , _ -> tr
    | (c , f) :: t ,  -> aux t (Lf c f)
    | (c , f) :: t , Lf c' f' ->
