#use "huffman_tree.ml"
module CharMap = Map.Make(Char);;

let read_file f = 
  let f_buff = open_in f in 
  try
    let line = input_line f_buff in 
    close_in f_buff; line
  with e -> close_in_noerr f_buff;
  raise e ;;

let write_bits filename bits =
  let oc = open_out_bin filename in
  let byte = ref 0 in
  let count = ref 0 in
  let flush_byte () =
    output_byte oc !byte;
    byte := 0;
    count := 0
  in
  List.iter (fun b ->
    byte := (!byte lsl 1) lor b;
    incr count;
    if !count = 8 then flush_byte ()
  ) l;
  if !count > 0 then (
    byte := !byte lsl (8 - !count);
    flush_byte ()
  );
  close_out oc
;;

let read_bits filename =
  let ic = open_in_bin filename in
  let rec read acc =
    match input_byte ic with
    | exception End_of_file -> close_in ic; List.rev acc
    | byte ->
        let bits =
          List.init 8 (fun i -> (byte lsr (7 - i)) land 1)
        in
        read (List.rev_append bits acc)
  in
  read []
;;

let file = "example.txt";;
let l, t = Huffman.encode (read_file file);;
let s = Huffman.decode l t ;;
write_bits "encoded.bin" l;;
let l2 = read_bits "encoded.bin";;
let s2 = Huffman.decode l2 t;;
