type 'a my_list =
	| Item of ('a * 'a my_list)
	| Empty
;;

(* List.print int *)
let rec print_list_int my_list =
	match my_list with
	| Empty -> ()
	| Item (hd, tl) -> print_int hd ; print_string " " ; print_list_int tl
;;

(* List.print str *)
let rec print_list_str my_list =
	match my_list with
	| Empty -> ()
	| Item (hd, tl) -> print_string hd ; print_string " " ; print_list_str tl
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
	| Empty -> raise (Failure "hd")
	| Item (hd, tl) -> hd
;;

(* List.tl *)
let rec tl my_list =
	match my_list with
	| Empty -> raise (Failure "tl")
	| Item (hd, tl) -> tl
;;

(* List.nth *)
let rec nth my_list idx =
	let len = length my_list in
	if idx < 0 then raise (Invalid_argument "List.nth")
	else if idx >= len then raise (Failure "nth")
	else
	match idx with
	| 0 -> (hd my_list)
	| _ -> nth (tl my_list) (idx - 1)
;;

(* List.rev *)
let rec rev my_list =
	let len = length my_list in
	match my_list with
	| 



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


let f = tl a;;
print_list_int f;;
print_endline "";;
let g = tl e;;
print_list_str g;;
print_endline "";;


let h = nth a 1;;
print_int h;;
print_endline "";;
let i = nth e 2;;
print_endline i;;
