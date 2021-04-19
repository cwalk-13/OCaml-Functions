(* 
// NAME: Charles Walker
// FILE: hw8b.ml
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


(* Testing *)
let assert_equal v1 v2 msg =
    let cond = v1 = v2 in
    assert (if not cond then print_endline ("TEST FAILED: " ^ msg) ; cond)
;;

(*Question 1: my_rev tests *)
assert_equal [] (my_rev []) "[] = my_rev []" ;;
assert_equal [3; 2; 1] (my_rev [1; 2; 3]) "[3; 2; 1] = my_rev [1; 2; 3]";;
assert_equal [4; 5; 6; 7] (my_rev [7; 6; 5; 4] ) "[4; 5; 6; 7] = my_rev [7; 6; 5; 4]";;

(*Question 2: my_last tests *)
assert_equal () (try my_last [] with _ -> ()) "my_last []" ;;
assert_equal 3 (my_last [1; 2; 3]) "3 = my_last [1; 2; 3]";;
assert_equal 1 (my_last [5; 4; 3; 23; 1]) "1 = my_last [5; 4; 3; 23; 1]";;

(*Question 3: my_init tests *)
assert_equal [] (my_init [] ) "[] = my_init []" ;;
assert_equal [1; 2] (my_init [1; 2; 3] ) "[1; 2] = my_init [1; 2; 3]";;
assert_equal [3; 4; 5] (my_init [3; 4; 5; 7] ) "[3; 4; 5] = my_init [3; 4; 5; 7]";;

(*Question 4: my_mem tests *)
assert_equal false (my_mem 1 [] ) "false = my_mem []" ;;
assert_equal true (my_mem 2 [1; 2; 3] ) "true = my_mem [1; 2; 3]";;
assert_equal false (my_mem 5 [1; 2; 3] ) "false = my_mem [1; 2; 3]";;

(*Question 5: my_replace tests *)
assert_equal [] (my_replace (2, 3) [] ) "[] = my_replace []" ;;
assert_equal [1; 8; 3; 8]  (my_replace (2,8)  [1; 2; 3; 2] ) "[1; 8; 3; 8] = my_replace [1; 2; 3; 2]";;
assert_equal [3; 3; 3; 1; 3]  (my_replace (5, 3) [5; 5; 5; 1; 5] ) "[3; 3; 3; 1; 3] = my_replace [5; 5; 5; 1; 5]";;

(*Question 6: my_replace_all tests *)
assert_equal [] (my_replace_all [] [] ) " [] = my_replace_all [] []" ;;
let pairs = [(1,2); (4,5)];;
let lst = [1; 2; 4; 4; 1; 1; 3];;
let lst1 = [2; 2; 5; 5; 2; 2; 3];;
assert_equal lst1 (my_replace_all pairs lst ) "[2; 2; 5; 5; 2; 2; 3] = my_replace_all pairs lst";;
assert_equal ['b'; 'b'; 'd'; 'd'] (my_replace_all  [('a','b');('c','d')] ['a'; 'b'; 'c'; 'd'] ) "['b'; 'b'; 'd'; 'd'] = my_replace_all  [('a','b');('c','d')] ['a'; 'b'; 'c'; 'd']";;

(*Question 7: my_elem_sum tests *)
assert_equal 0 (my_elem_sum 1 [] ) " 0 = my_elem_sum 1 []" ;;
assert_equal 0 (my_elem_sum 1 [3; 4; 5] ) " 0 = my_elem_sum 1 [3; 4; 5]" ;;
assert_equal 12 (my_elem_sum 3 [3; 2; 3; 2; 3; 4; 3] ) " 12 = my_elem_sum 3 [3; 2; 3; 2; 3; 4; 3]";;

(*Question 8: my_rem_dups tests *)
assert_equal [] (my_rem_dups [] ) " [] = my_rem_dups []" ;;
assert_equal ['c'; 'b'; 'a'] (my_rem_dups ['a'; 'b'; 'a'; 'c'; 'b'; 'a'] ) " ['c'; 'b'; 'a'] = my_rem_dups ['a'; 'b'; 'a'; 'c'; 'b'; 'a'] " ;;
assert_equal [12; 13; 11; 10] (my_rem_dups  [10; 11; 13; 11; 12] ) " [12; 13; 11; 10] = my_rem_dups  [10; 11; 13; 11; 12]" ;;

(*Question 9: my_min tests *)
assert_equal () (try my_min [] with _ -> ()) "my_min []" ;;
assert_equal 1 (my_min [7; 1; 9; 12; 10] ) "1 = my_min [7; 1; 9; 12; 10]" ;;
assert_equal 2 (my_min [5; 20; 3; 2] ) "2 = my_min [5; 20; 3; 2]" ;;