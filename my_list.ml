type 'a my_list =
	| Item of ('a * 'a my_list)
	| Empty
;;


(* List.length *)
let rec length my_list =
	match my_list with
	| Empty -> 0
       	| Item (hd, tl) -> 1 + length (tl)
;;

(* List.hd *)
let rec hd my_list =
	match my_list with
	| Empty -> 0 (* throw exception *)
	| Item (hd, tl) -> hd

let a = Item(1, Item( 2, Item(3, Empty)));;
let e = Item("a", Item("b", Item("c", Empty)));;

let b = length a;;
print_int b;;
print_endline "";;


let c = hd a;;
print_int c;;
print_endline "";;
let d = hd e;;
print_endline d;;
