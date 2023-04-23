val empty = [];

val list1 = [1, 2, 3]

val list2 = [true, false, true]

(* [2 + 3, false] error *)

val list3 = 4 :: [1, 2, 3]  (* [4, 1, 2, 3] *)

val list4 = [1, 2, 3] @ [4, 5] (* [1, 2, 3, 4, 5] *)

val isEmpty = null empty

val a = hd list1

val list5 = tl list1