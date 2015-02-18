type 'a my_list =
	| Item of ('a * 'a my_list)
	| Empty
;;


(* List length *)
let rec length my_list =
	match my_list with
	| Empty -> 0
       	| Item (hd, tl) -> 1 + length (tl)
;;


let a = Item("salope", Item("encule", Item("biatch", Empty)));;
let b = length a;;

print_int b;;
print_endline "";;
