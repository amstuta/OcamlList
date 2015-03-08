(* Definition du type *)
type 'a my_list =
  | Item of ('a * 'a my_list)
  | Empty

      
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
  if idx < 0 then raise (Invalid_argument "My_list.nth")
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
    | Empty	    -> elem
    | Item (hd, tl) -> flatten_in (append elem hd) tl
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
       if (f hd) = true then for_all f tl
       else false
     end
       
(* List.exists *)
let rec exists f = function
  | Empty	  -> false
  | Item (hd, tl) ->
     begin
       if (f hd) = true then true
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
	 if (f hd) = true then filter_in (Item (hd, elem)) tl
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
	 if a = key then append e tl
	 else remove_assoc_in (Item (hd, e)) tl
       end
  in remove_assoc_in Empty my_list


		     
(* ------------------------ BONUS -------------------------- *)

(* List.fold_right *)
let fold_right f a my_list =
  fold_left f a (rev my_list)

(* List.iter2 *)
let rec iter2 f l1 l2 =
  if (length l1) <> (length l2) then raise (Invalid_argument "My_list.iter2")
  else match l1 with
    | Empty       -> ()
    | Item (h, t) ->
       begin
	 f h (hd l2);
	 iter2 f t (tl l2);
       end

(* List.map2 *)
let map2 f l1 l2 =
  let len1 = length l1 in
  let len2 = length l2 in
  if len1 <> len2 then raise (Invalid_argument "My_list.map2")
  else
    let rec map2_in elem il2 = function
      | Empty  	    -> rev elem
      | Item (h, t) -> map2_in (Item ((f h (hd il2)), elem)) (tl il2) t
    in map2_in Empty l2 l1

(* List.partition *)
let partition f my_list =
  let rec partition_in elem = function
    | Empty	    -> elem
    | Item (hd, tl) ->
       begin
	 let b = f hd in
	 let lT = (function (a, b) -> a) elem in
	 let lF = (function (a, b) -> b) elem in
	 if b = true then partition_in ((Item (hd, lT)), lF) tl
	 else partition_in (lT, (Item (hd, lF))) tl
       end
  in partition_in (Empty, Empty) my_list

(* List.mem_assq *)
let rec mem_assq key = function
  | Empty	  -> false
  | Item (hd, tl) ->
     begin
       let a = (function (e, _) -> e) hd in
       if a == key then true
       else mem_assq key tl
     end
       
(* List.remove_assq *)
let remove_assq key my_list =
  let rec remove_assq_in e = function
    | Empty	    -> rev e
    | Item (hd, tl) ->
       begin
	 let a = (function (a, b) -> a) hd in
	 if a == key then append e tl
	 else remove_assq_in (Item (hd, e)) tl
       end
  in remove_assq_in Empty my_list

(* List.remove_all_assoc *)
let remove_all_assoc key my_list =
  let rec remove_all_assoc_in e = function
    | Empty	    -> rev e
    | Item (hd, tl) ->
       begin
	 let a = (function (a, b) -> a) hd in
	 if a = key then remove_all_assoc_in e tl
	 else remove_all_assoc_in (Item (hd, e)) tl
       end
  in remove_all_assoc_in Empty my_list;;
