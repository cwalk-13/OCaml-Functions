(* 
// NAME: Charles Walker
// FILE: hw8a.ml
// DATE: 4/9/2021
// DESC: Ocaml list functions using pattern matching
*)

(**to print a list *)
let rec print_xs xs =
    if  List.length xs > 0 then
        let str = string_of_int(List.hd xs) in
        print_string str;
        print_xs (List.tl xs)
    else
        print_string "\n"
;;

(*Question 1: my_rev: 'a list -> 'a list*)
let rec my_rev xs =
    match xs with 
    | [] -> [] 
    | h::t -> my_rev t @ [h]
;;

(*Question 2: my_last: 'a list -> 'a*)
let rec my_last xs =
    match xs with
    | [] -> failwith "Empty List"
    | [x] -> x
    | _::t -> my_last t
;;

(*Question 3: my_init: 'a list -> 'a list *)
let rec my_init xs =
    match xs with 
        | [] -> []
        | [x] -> []
        | h::t -> h :: my_init t
;;

(*Question 4: my_mem: 'a -> 'a list -> bool*)
let rec my_mem x xs =
    match xs with
    | [] -> false 
    | h::t when h == x -> true
    | _::t -> my_mem x t
;;

(*Question 5: my_replace: 'a * 'a -> 'a list -> 'a list *)
let rec my_replace pair xs =
    match xs with
    | [] -> []
    | h::t when h == fst(pair) -> snd(pair) :: my_replace pair t
    | h::t -> h::my_replace pair t
;;

(*Question 6: my_replace_all: ('a * 'a) list -> 'a list -> 'a list *)
let rec my_replace_all ps xs =
    match ps with
    | [] -> xs
    | h::t -> my_replace_all t (my_replace h xs)
;;

(*Question 7: my_elem_sum: int -> int list -> int *)
let rec my_elem_sum x xs =
    match xs with 
    | [] -> 0 
    | h::t when h == x -> x + my_elem_sum x t 
    | _::t -> 0 + my_elem_sum x t 
;;

(*Question 8: my_rem_dups: 'a list -> 'a list *)
let my_rem_dups xs =
    let rec my_rem_dups_rec dups xs =
        match xs with
        | [] -> dups
        | h::t when my_mem h dups -> my_rem_dups_rec dups t
        | h::t -> my_rem_dups_rec (h::dups) t 
    in my_rem_dups_rec [] xs 
;;

(*Question 9: my_min: 'a list -> 'a *)
let my_min xs = 
    let rec my_min_rec min xs =
        match xs with
        | [] -> min
        | h::t when h < min -> my_min_rec h t 
        | _::t -> my_min_rec min t 
    in match xs with
    | [] -> failwith "Empty List"
    | h::t -> my_min_rec h xs 
;;

(*Question 1: my_rev tests *)

(*Question 2: my_last tests *)
assert_equal () (try my_last [] with _ -> ())
    "my_last []" ;;

(*Question 3: my_init tests *)

(*Question 4: my_mem tests *)

(*Question 5: my_replace tests *)

(*Question 6: my_replace_all tests *)

(*Question 7: my_elem_sum tests *)

(*Question 8: my_rem_dups tests *)

(*Question 9: my_min tests *)