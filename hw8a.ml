(* 
// NAME: Charles Walker
// FILE: hw8a.ml
// DATE: 4/9/2021
// DESC: Ocaml list functions without the use of pattern matching
*)

(**to print a list *)
let rec print_xs xs =
    if  List.length xs > 0 then
        let str = string_of_int(List.hd xs) in
        print_string str;
        print_xs (List.tl xs)
    else print_string "\n"
;;


(*Question 1: my_rev: 'a list -> 'a list*)
let rec my_rev xs =
    let xs2 = (List.hd xs) :: [] in
    my_rev_rec (List.tl xs) xs2
and my_rev_rec xs rev_xs =
    if List.length xs > 0 then
        let rev_xs2 = (List.hd xs) :: rev_xs in
        my_rev_rec (List.tl xs) rev_xs2
    else rev_xs
;;
let xs = my_rev [1; 2; 3];;
print_xs xs;;

(*Question 2: my_last: 'a list -> 'a*)
let rec my_last xs =
    if List.length xs > 1 then
        let xs2 = (List.tl xs) in
        my_last xs2
    else if List.length xs == 1 then List.hd xs
    else failwith "Empty List"
;;

let num = my_last [1; 2; 3; 4];;
let num2 = string_of_int(num) ;;
print_endline num2;;

(*Question 3: my_init: 'a list -> 'a list *)
let my_init xs =
    let xs1_rev = my_rev xs in 
    my_rev (List.tl xs1_rev)

let xs = my_init [1; 2; 3];;
print_xs xs;;

(*Question 4: my_mem: 'a -> 'a list -> bool*)
let rec my_mem x xs =
    if List.hd xs != x then
        if List.length xs > 1 then
            my_mem x (List.tl xs)
        else 
            false
    else 
        true
;;

let mem = my_mem 5 [1; 2; 3; 4];;
let mem2 = string_of_bool(mem) ;;
print_endline mem2;;

(*Question 5: my_replace: 'a * 'a -> 'a list -> 'a list *)
let rec my_replace pair xs =
    if List.length xs == 0 then xs
    else if List.hd xs == fst(pair) then snd(pair) :: my_replace pair (List.tl xs)
    else List.hd xs :: my_replace pair (List.tl xs)
;;
let xs = my_replace (2,8)  [1; 2; 3; 2];;
print_xs xs;;

(*Question 6: my_replace_all: ('a * 'a) list -> 'a list -> 'a list *)
let rec my_replace_all ps xs =
    if List.length ps == 0 then xs
    else my_replace_all (List.tl ps) (my_replace(List.hd ps) xs)
        

let xs = my_replace_all [(1,2); (2,3)] [1; 2; 3; 4] ;;
print_xs xs;;

(*Question 7: my_elem_sum: int -> int list -> int *)
let rec my_elem_sum x xs =
    if List.length xs == 0 then 0
    else my_elem_sum_rec x xs 0
and my_elem_sum_rec x xs sum =
    if List.length xs == 0 then sum
    else if List.hd xs == x then my_elem_sum_rec x (List.tl xs) ((fun y -> x + y )sum)
    else my_elem_sum_rec x (List.tl xs) (sum)
    
let num = my_elem_sum 3 [3; 2; 3; 2; 3; 4; 3];;
let num2 = string_of_int(num) ;;
print_endline num2;;

(*Question 8: my_rem_dups: 'a list -> 'a list *)
let my_rem_dups xs =
    let rec my_rem_dups_rec dups xs =
        if List.length xs == 0 then dups
        else if my_mem (List.hd xs) dups then my_elem_sum_rec dups (List.tl xs)
        else my_elem_sum_rec (List.hd xs :: dups) (List.tl xs)
    in my_elem_sum_rec ([]) xs
;;

(*Question 9: my_min: 'a list -> 'a *)
let my_min xs =
    if List.length xs > 0 then
        let rec my_min_rec min xs = 
            if List.length == 0 then min
            else if List.h xs < min then my_min_rec (List.hd xs) (List.tl xs)
            else my_min_rec min (List.tl xs)
        in my_min_rec (List.hd xs) xs
    else failwith "Empty List"
;;


(*Question 1: my_rev tests *)

(*Question 2: my_last tests *)


(*Question 3: my_init tests *)

(*Question 4: my_mem tests *)

(*Question 5: my_replace tests *)

(*Question 6: my_replace_all tests *)

(*Question 7: my_elem_sum tests *)

(*Question 8: my_rem_dups tests *)

(*Question 9: my_min tests *)
