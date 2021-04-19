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

(*Question 2: my_last: 'a list -> 'a*)
let rec my_last xs =
    if List.length xs > 1 then
        let xs2 = (List.tl xs) in
        my_last xs2
    else if List.length xs == 1 then List.hd xs
    else failwith "Empty List"
;;


(*Question 3: my_init: 'a list -> 'a list *)
let my_init xs =
    let xs1_rev = my_rev xs in 
    my_rev (List.tl xs1_rev)
;;

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


(*Question 5: my_replace: 'a * 'a -> 'a list -> 'a list *)
let rec my_replace pair xs =
    if List.length xs == 0 then xs
    else if List.hd xs == fst(pair) then snd(pair) :: my_replace pair (List.tl xs)
    else List.hd xs :: my_replace pair (List.tl xs)
;;


(*Question 6: my_replace_all: ('a * 'a) list -> 'a list -> 'a list *)
let rec my_replace_all ps xs =
    if List.length ps == 0 then xs
    else my_replace_all (List.tl ps) (my_replace(List.hd ps) xs)
;;

(*Question 7: my_elem_sum: int -> int list -> int *)
let rec my_elem_sum x xs =
    if List.length xs == 0 then 0
    else my_elem_sum_rec x xs 0
and my_elem_sum_rec x xs sum =
    if List.length xs == 0 then sum
    else if List.hd xs == x then my_elem_sum_rec x (List.tl xs) ((fun y -> x + y )sum)
    else my_elem_sum_rec x (List.tl xs) (sum)
;;

(*Question 8: my_rem_dups: 'a list -> 'a list *)
let my_rem_dups xs =
    let rec my_rem_dups_rec dups xs =
        if List.length xs == 0 then dups
        else if my_mem (List.hd xs) dups then my_rem_dups_rec dups (List.tl xs)
        else my_rem_dups_rec (List.hd xs :: dups) (List.tl xs)
    in my_rem_dups_rec [] xs
;;

(*Question 9: my_min: 'a list -> 'a *)
let my_min xs =
    if List.length xs > 0 then
        let rec my_min_rec min xs = 
            if List.length xs == 0 then min
            else if List.hd xs < min then my_min_rec (List.hd xs) (List.tl xs)
            else my_min_rec min (List.tl xs)
        in my_min_rec (List.hd xs) xs
    else failwith "Empty List"
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
