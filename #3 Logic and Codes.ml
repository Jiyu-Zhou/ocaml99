(*** 99 Problems in Ocaml ***)

(** Logic and Codes **)

#use "#2 Arithmetic.ml";;

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr;;

(* 40th *)
let rec eval2 a str_a b str_b expr =
  match expr with
  | Var v ->
     let (x , bx) = str_a in
     let (y , by) = str_b in
     if x = v then bx
     else if y = v then by
     else assert false
  | Not x -> not (eval2 a str_a b str_b x)
  | And(x1 , x2) -> eval2 a str_a b str_b x1 && eval2 a str_a b str_b x2
  | Or(x1 , x2) -> eval2 a str_a b str_b x1 || eval2 a str_a b str_b x2;;

let table2 a b expr =
  [(true , true , eval2 a (a , true) b (b , true) expr)
  ;(true , false , eval2 a (a , true) b (b , false) expr)
  ;(false , true , eval2 a (a , false) b (b , true) expr)
  ;(false , false , eval2 a (a , false) b (b , false) expr)];;

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
