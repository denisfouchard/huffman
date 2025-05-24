module CharMap = Map.Make(Char);;

module PriorityQueue = struct 
  type 'a queue = ('a * int) list

  let empty : 'a queue = []
  let rec insert (x: 'a *int) (q:'a queue) : 'a queue =
    let c, i = x in
    match q with 
    | [] -> [x]
    | (c1, i1)::l ->
        if i >= i1 then x::q
        else (c1, i1)::(insert x l)

  let of_list (l: ('a*int) list) : 'a queue =
    let rec insert_all (l) (q:'a queue) = 
      match l with
      | [] -> q
      | t::lq -> insert t (insert_all lq q)
    in insert_all l empty
    
  let of_char_map (map:int CharMap.t) : 'a queue = 
    let map_list = CharMap.to_list map in
    of_list map_list
  
  let pop (q: 'a queue) : ('a *int) option * 'a queue =
    match q with
    | [] -> (None, q)
    | t::lq -> (Some t, lq)

  let length (q : 'a queue) : int =
    List.length q
  

  let is_empty (q : 'a queue) : bool =
    List.is_empty q
  end
;;
