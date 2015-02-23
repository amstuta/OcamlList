(* Definition du type *)
type 'a my_list =
  | Item of ('a * 'a my_list)
  | Empty

      

(* List.print int *)
let rec print_list_int my_list =
  match my_list with
  | Empty -> ()
  | Item (hd, tl) ->
     begin
       print_int hd;
       print_string " ";
       print_list_int tl;
     end

(* List.print str *)
let rec print_list_str my_list =
  match my_list with
  | Empty -> ()
  | Item (hd, tl) -> print_string hd ; print_string " " ; print_list_str tl

(* Prints a tuple containing lists *)
let print_tuple_of_list my_tuple =
  let a = (function (a, b) -> a) my_tuple in
  let b = (function (a, b) -> b) my_tuple in
  print_list_int a;
  print_endline "";
  print_list_int b

(* Prints a list containing tuples *)
let rec print_list_of_tuple my_list =
  match my_list with
  | Empty -> ()
  | Item (hd, tl) ->
     begin
       let a = (function (a, b) -> a) hd in
       let b = (function (a, b) -> b) hd in
       print_string "(";
       print_int a;
       print_string ", ";
       print_int b;
       print_string ")  ";
       print_list_of_tuple tl
     end

(* Add a et b *)
let add a b = a + b;;

(* Add 1 *)
let add_un a = a + 1;;

(* Renvoie true si diff *)
let diff_0 a = a <> 0;;



      
(* List.length *)
let rec length = function
  | Empty	  -> 0
  | Item (hd, tl) -> 1 + length tl
		
(* List.hd *)
let rec hd = function
  | Empty	  -> raise (Failure "hd")
  | Item (hd, tl) -> hd
	       
(* List.tl *)
let rec tl = function
  | Empty	  -> raise (Failure "tl")
  | Item (hd, tl) -> tl
	       
(* List.nth *)
let rec nth my_list idx =
  let len = length my_list in
  if idx < 0 then raise (Invalid_argument "List.nth")
  else if idx >= len then raise (Failure "nth")
  else
    match idx with
    | 0 -> (hd my_list)
    | _ -> nth (tl my_list) (idx - 1)

(* List.rev *)
let rev my_list =
  let rec rev_in t = function
    | Empty	    -> t
    | Item (hd, tl) -> rev_in (Item (hd, t)) tl
  in rev_in Empty my_list

(* List.append *)
let append l1 l2 =
  let rec append_in nl = function
    | Empty	    -> nl
    | Item (hd, tl) -> append_in (Item (hd, nl)) tl
  in append_in l2 (rev l1)

(* List.rev_append *)
let rev_append l1 l2 =
  let rec rev_append_in nl = function
    | Empty	    -> nl
    | Item (hd, tl) -> rev_append_in (Item (hd, nl)) tl
  in rev_append_in l2 l1

(* List.flatten *)
let flatten my_list =
  let rec flatten_in elem = function
    | Empty	    -> rev elem
    | Item (hd, tl) -> flatten_in (rev_append hd elem) tl
  in flatten_in Empty my_list

(* List.iter *)
let rec iter f = function
  | Empty	  -> ()
  | Item (hd, tl) ->
     begin
       f hd;
       iter f tl;
     end

(* List.map *)
let map f my_list =
  let rec map_in elem = function
    | Empty	    -> rev elem
    | Item (hd, tl) -> map_in (Item ((f hd), elem)) tl
  in map_in Empty my_list
       
(* List.fold_left *)
let fold_left f a my_list =
  let rec fold_left_in i = function
    | Empty	    -> i
    | Item (hd, tl) -> fold_left_in (f  i hd) tl
  in fold_left_in a my_list

(* List.for_all *)
let rec for_all f = function
  | Empty	  -> true
  | Item (hd, tl) ->
     begin
       let b = f hd in
       if b = true then for_all f tl
       else false
     end
       
(* List.exists *)
let rec exists f = function
  | Empty	  -> false
  | Item (hd, tl) ->
     begin
       let b = f hd in
       if b = true then true
       else exists f tl
     end
       
(* List.mem *)
let rec mem a = function
  | Empty	  -> false
  | Item (hd, tl) ->
     begin
       let b = (a = hd) in
       if b = true then true
       else mem a tl
     end

(* List.memq *)
let rec memq a = function
  | Empty	  -> false
  | Item (hd, tl) ->
     begin
       let b = (a == hd) in
       if b = true then true
       else memq a tl
     end

(* List.filter *)
let filter f my_list =
  let rec filter_in elem = function
    | Empty	    -> rev elem
    | Item (hd, tl) ->
       begin
	 let b = f hd in
	 if b = true then filter_in (Item (hd, elem)) tl
	 else filter_in elem tl
       end
  in filter_in Empty my_list

(* List.mem_assoc *)
let rec mem_assoc key = function
  | Empty	  -> false
  | Item (hd, tl) ->
     begin
       let a = (function (e, _) -> e) hd in
       if a = key then true
       else mem_assoc key tl
     end

(* List.assoc *)
let rec assoc key = function
  | Empty	  -> raise Not_found
  | Item (hd, tl) ->
     begin
       let a = (function (e, f) -> e) hd in
       if a = key then
	 let b = (function (e, f) -> f) hd in b
       else assoc key tl
     end

(* List.split *)
let split my_list =
  let rec split_in e f = function
    | Empty	    -> ((rev e), (rev f))
    | Item (hd, tl) ->
       begin
	 let a = (function (a, b) -> a) hd in
	 let b = (function (a, b) -> b) hd in
	 split_in (Item (a, e)) (Item (b, f)) tl
       end
  in split_in Empty Empty my_list

(* List.remove_assoc *)
let remove_assoc key my_list =
  let rec remove_assoc_in e = function
    | Empty	    -> rev e
    | Item (hd, tl) ->
       begin
	 let a = (function (a, b) -> a) hd in
	 if a = key then remove_assoc_in e tl
	 else remove_assoc_in (Item (hd, e)) tl
       end
  in remove_assoc_in Empty my_list


let main =
  let a = Item(0, Item(1, Item(2, Item(3, Empty)))) in
  let e = Item("a", Item("b", Item("c", Empty))) in

	    
  print_endline "Test length:";
  let b = length a in
  print_int b;
  print_endline "";


  print_endline "Test hd:";
  let c = hd a in
  print_int c;
  print_endline "";
  let d = hd e in
  print_endline d;


  print_endline "Test tl:";
  let f = tl a in
  print_list_int f;
  print_endline "";
  let g = tl e in
  print_list_str g;
  print_endline "";


  print_endline "Test nth:";
  let h = nth a 1 in
  print_int h;
  print_endline "";
  let i = nth e 2 in
  print_endline i;


  print_endline "Test rev:";
  let p = rev a in
  print_list_int p;
  print_endline "";


  print_endline "Test append:";
  let r = Item(4, Item(5, Item(6, Empty))) in
  let s = append a r in
  print_list_int s;
  print_endline "";

  
  print_endline "Test rev_append:";
  let q = rev_append a r in
  print_list_int q;
  print_endline "";

  print_endline "Test flatten:";
  let t = Item("a", Item("b", Item("c", Empty))) in
  let u = Item("d", Item("e", Item("f", Empty))) in
  let v = Item(t, Item(u, Empty)) in
  let w = flatten v in
  print_list_str w;
  print_endline "";
  
  print_endline "Test iter:";
  iter print_int a;
  print_endline "";

  print_endline "Test map:";
  let x = map add_un a in
  print_list_int x;
  print_endline "";
  
  print_endline "Test fold_left:";
  let k = fold_left add 9 a in
  print_int k;
  print_endline "";

  print_endline "Test for_all:";
  let l = for_all diff_0 a in
  print_endline (string_of_bool l);
  
  print_endline "Test exists:";
  let m = exists diff_0 a in
  print_endline (string_of_bool m);
  
  print_endline "Test mem:";
  print_endline (string_of_bool (mem 0 a));
  print_endline (string_of_bool (mem 15 a));
  
  print_endline "Test memq:";
  let n = "salut" in
  let o = Item(n, Item("tout le monde", Empty)) in
  print_endline (string_of_bool (memq n o));
  print_endline (string_of_bool (memq "salut" o));

  print_endline "Test filter:";
  let y = filter diff_0 a in
  print_list_int y;
  print_endline "";
  
  print_endline "Test mem_assoc:";
  let z = Item((1, 2), Item((3,4), Empty)) in
  print_endline (string_of_bool (mem_assoc 3 z));

  print_endline "Test assoc:";
  print_int (assoc 1 z);
  print_endline "";

  print_endline "Test split:";
  let ab = split z in
  print_tuple_of_list ab;
  print_endline "";

  print_endline "Test remove_assoc:";
  let ac = remove_assoc 3 z in
  print_list_of_tuple ac;
  print_endline "";
