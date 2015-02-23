open My_list
       
(* List.print int *)
let rec print_list_int = function
  | Empty	  -> ()
  | Item (hd, tl) ->
     begin
       print_int hd;
       print_string " ";
       print_list_int tl;
     end

(* List.print str *)
let rec print_list_str = function
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
