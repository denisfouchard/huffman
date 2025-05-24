#use "priority_queue.ml";; 
module CharMap = Map.Make(Char);;
let string_of_char = String.make 1;;


let occurences (text : string) = 
  let occ_map = CharMap.empty in
  let n = String.length text in 
  let rec occurences_aux i map = 
    if i == -1 
      then map
    else 
      let char = text.[i] in
      let new_map = 
        match CharMap.find_opt char map with
        | Some k -> (CharMap.add char (k+1) map)
        | None -> (CharMap.add char 1 map)
      
      in occurences_aux (i-1) new_map
  in occurences_aux (n-1) occ_map;;
        

module HuffmanTree = struct

  type huffmantree = 
    | Leaf of char
    | Node of huffmantree * huffmantree

  let decode (encoded:int list) (huff_tree:huffmantree) : string = 
    let rec decode_acc seq tree acc = 
      match seq with
      | [] -> 
        (match tree with
          | Leaf x -> acc^(string_of_char x)
          | _ -> acc)
      | t::q ->
          (match tree with
            | Leaf x -> 
              decode_acc seq huff_tree (acc^(string_of_char x))
            | Node (l, r) -> 
              decode_acc q (if t = 0 then l else r) acc)
    in decode_acc encoded huff_tree ""
    
  let rec _of_priority_queue (queue: huffmantree PriorityQueue.queue) : huffmantree = 
      if PriorityQueue.is_empty queue then Leaf 'a'
      else if PriorityQueue.length queue = 1 then
        let (Some (tx, _), _) = PriorityQueue.pop queue in tx
      else
        let (Some (t1, i1), q1) = PriorityQueue.pop queue in
        let (Some (t2, i2), q2) = PriorityQueue.pop q1 in
        let new_t = Node (t1, t2) in
      _of_priority_queue (PriorityQueue.insert (new_t, i1+i2) q2)
    
  let of_char_map (map:int CharMap.t) : huffmantree = 
    let list_map = CharMap.to_list map in
      let rec insert_all (l) (q:huffmantree PriorityQueue.queue) :huffmantree PriorityQueue.queue = 
        match l with
        | [] -> q
        | (t, i)::lq -> PriorityQueue.insert ((Leaf t), i) (insert_all lq q)
      in 
        let huff_tree_queue = insert_all list_map PriorityQueue.empty in
  _of_priority_queue huff_tree_queue

  let create_codemap (tree:huffmantree) : int list CharMap.t =
    let code_map = CharMap.empty in
    let rec explore (t:huffmantree) (map: int list CharMap.t) (acc : int list) : (int list CharMap.t) =
      match t with
      | Leaf x -> (CharMap.add x (List.rev acc) map)
      | Node (left, right) ->
          let map1 = explore left map (0::acc) in
          explore right map1 (1::acc)
      in explore tree code_map []

  let encode_string (text: string) (code_map : int list CharMap.t) : int list =
    let rec encode_acc i  =
      if i = -1 
        then []
      else let x = text.[i] in
            let l =  (CharMap.find x code_map) in 
            (encode_acc (i-1)) @ l 
    in encode_acc (String.length text -1)


  let encode (text: string) : (int list) * huffmantree =
    let occ_map = occurences text in
    let encoding_tree = of_char_map occ_map in
    let code_map = create_codemap encoding_tree in
    (encode_string text code_map) , encoding_tree

end
;;


