(*** 99 Problems in OCaml ***)

(** Arithmetic **)

#use "#1 Working with lists.ml";;

(* 29th *)
let is_prime n =
  let rec aux m k =
    if m <= 1 then false
    else if k = m then true
    else if k * k > m then true
    else if m mod k = 0 then false
    else aux m (k + 1)
  in
  aux n 2;;

(* 30th *)
let rec gcd n m =
  if n = 1 then 1
  else if n = m then m
  else if n < m then gcd n (m - n)
  else gcd m n;;

(* 31st *)
let rec coprime n m =
  if gcd n m = 1 then true
  else false;;

(* 32nd *)
let phi n =
  let rec aux m k i =
    if m = 1 then 1
    else if m = k then i
    else if coprime m k then aux m (k + 1) (i + 1)
    else aux m (k + 1) i
  in
  aux n 1 0;;

(* 33rd *)
let factors n =
  let rec aux n k l =
    if k > n then l
    else if n mod k = 0 && is_prime k then k :: (aux (n / k) k l)
    else aux n (k + 1) l
  in
  aux n 1 [];;

(* 34th *)
let rec factors' n =
  let aux = function
    | One x -> (x , 1)
    | Many (m , x) -> (x , m)
  in
  List.map aux (encode'' (factors n));;

(* 35th *)
let phi_improved n =
  let rec aux n m =
    if m = 0 then 1
    else if m = 1 then (n - 1)
    else aux (n * m) (m - 1)
  in
  List.fold_left ( * ) 1
                 (List.map (fun (x , y) -> (x - 1) * (aux x (y - 1))) (factors' n));;

(* 36th *)
  (* (phi_improved) *)

(* 37th *)
let all_primes n m =
  let rec aux a i =
    if i > a then []
    else if is_prime i then i :: (aux a (i + 2))
    else aux a (i + 2)
  in
  if n = 2 then 2 :: (aux m 3) else aux m n;;

(* 38th *)
let rec is_even n =
  if n = 0 then true
  else if n = 1 then false
  else is_even (n - 2);;

exception NotEven;;
let goldbach n =
  let rec aux m i =
    if is_prime i && is_prime (m - i) then (i , m - i)
    else aux m (i + 2)
  in
  if not (is_even n) then raise NotEven
  else if is_prime (n - 2) then (2 , n - 2)
  else aux n 3;;

(* 39th *)
let even_gen n m =
  let rec aux i a =
    if i > a then []
    else i :: (aux (i + 2) a)
  in
  if n < 4 then aux 4 m
  else if not (is_even n) then aux (n + 1) m
  else aux n m;;

let goldbach_list n m =
  let aux a =
    (a , goldbach a)
  in
  List.map aux (even_gen n m);;

let goldbach_limit n m limit =
  List.filter
    (fun x -> ((fst (snd x)) > limit) && ((snd (snd x)) > limit))
    (goldbach_list n m);;
