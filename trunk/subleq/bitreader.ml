(* =============================================================== *)
(** Lecture bit à bit : Projet SubLeq *)
(* =============================================================== *)

(* le bit est un boolean *)

(* On creer un type abstrait : bit_channel 
   et le module qui sert de barriere d'abstraction *)

(* Rappel : 8 bits font un octet *)

(* obtenir les octets des faibles vers les forts a partir d'un char *)
(* En lecture bit a bit, on concidere que les octets sont lus dans quel sens ? *)


let bits_of_char q c = 
  let n = ref (int_of_char c) in
  for i = 0 to 7 do
    Queue.push (!n mod 2 = 1) q;
    n := (!n) / 2;
  done;


module Bit =    
struct
  type bit_in_channel = { ic : in_channel; bits : bool Queue.t }
  type bit_out_channel = { oc : out_channel; mutable accu : int; mutable length : int } 
 
  let bopen_in filename = 
    let ic = open_in filename in
    let q = Queue.create () in
    { ic = ic; bits = q }

  let bopen_out filename = 
    let oc = open_out filename in
    { oc = oc; accu = 0; length = 0 }

  let bread bic = 
    if Queue.is_empty bic.bits 
    then
      let octet = input_char bic.ic in
      bits_of_char bic.bits octet
    else ();
    Queue.pop bic.bits

  let bwrite boc data = 
    if boc.length = 8
    then
      begin
        output_char boc.oc (char_of_int boc.accu);
        boc.accu <- 0;
        boc.length <- 0
      end
    else ();
    (* Write data *)
    boc.accu <- boc.accu * 2 + (if data then 1 else 0);
    boc.length <- succ boc.length

    let bflush boc = 
      if boc.length = 8 
      then 
        begin
          output_char boc.oc (char_of_int boc.accu);
          boc.accu <- 0;
          boc.length <- 0
        end
      else ()

    let bclose_out boc = bflush boc; close_out boc.oc
    let bclose_in bic = close_in bic.ic
  
end;;



let mega_test () =
  let file = "test01" in
  let len = 128 in
  for i = 0 to 200 do
    let tab = Array.init len (fun _ -> Random.bool ()) in
    begin
      let boc = Bit.bopen_out file in
      for j = 0 to pred len do
        Bit.bwrite boc tab.(j)
      done;
      Bit.bclose_out boc;
      
      let bic = Bit.bopen_in file in
      for j = 0 to pred len do
        let br = Bit.bread bic in
        if br <> tab.(j) 
        then failwith (Printf.sprintf "Erreur : bit %d : ")
      done;
      
      end
  done;;

    
let boc = Bit.bopen_out "test01";;
Bit.bclose_out boc;;

let bic = Bit.bopen_in "test01";;

Bit.bread bic;;
