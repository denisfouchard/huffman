#use "huffman_tree.ml"
module CharMap = Map.Make(Char);;


let file = "example.txt";;

let read_file f = 
  let f_buff = open_in f in 
  try
    let line = input_line f_buff in 
    close_in f_buff; line
  with e -> close_in_noerr f_buff;
  raise e ;;



let l, t = HuffmanTree.encode (read_file file);;
let s = HuffmanTree.decode l t ;;