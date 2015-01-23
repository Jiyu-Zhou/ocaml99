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
type ('a , 'b) tree =
  | Lf of 'a * 'b
  | Br of ('a , 'b) tree * 'b * ('a , 'b) tree;;

let merge tr1 tr2 =
  match tr1, tr2 with
  | Lf (_ , n1), Lf (_ , n2) -> Br (tr1, (n1 + n2), tr2)
  | Lf (_ , n1), Br (_ , n2, _) -> Br (tr1, (n1 + n2), tr2)
  | Br (_ , n1 , _), Lf (_ , n2) -> Br (tr1, (n1 + n2), tr2)
  | Br (_ , n1 , _), Br (_ , n2, _) -> Br (tr1, (n1 + n2), tr2);;

let get_int x =
  match x with
  | Lf (_ , i) -> i
  | Br (_ , i, _) -> i;;

let rec sort list =
  let rec divide x list =
    match list with
    | [] -> ([] , [])
    | h :: t -> if get_int h < x
               then (h :: (fst (divide x t)) , snd (divide x t))
               else (fst (divide x t) , h :: (snd (divide x t)))
  in
  match list with
  | [] -> []
  | h :: t -> let i = get_int h in
             (sort (fst (divide i t)))
             @ [h]
             @ (sort (snd (divide i t)));;

let rec huffman_tree = function
    | [] -> assert false
    | x :: [] -> x
    | x :: y :: t -> huffman_tree (sort ((merge x y) :: t));;

let huffman list =
  let tr = huffman_tree (sort (List.map (fun (x , y) -> Lf (x , y)) list)) in
  let rec aux at = function
  | Lf(x, _) -> [(x, at)]
  | Br(l, _, r) -> aux ("0" :: at) l @ aux ("1" :: at) r
  in
  List.map (fun (x , y) -> (x , String.concat "" (List.rev y))) (aux [] tr);;
