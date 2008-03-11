(* =============================================================== *)
(** Lecture bit à bit : Projet SubLeq *)
(* =============================================================== *)

open TestUnit

(* 
   le bit est un boolean 

   On creer un type abstrait : bit_channel 
   et le module qui sert de barriere d'abstraction 

   Rappel : 8 bits font un octet 

   convention : obtenir les octets des faibles vers les forts a 
   partir d'un char 
*)

(* =============================================================== *)
(** exponentiation infixe naive pour des petites puissances de 2 *)
(* =============================================================== *)
let (^$) a = 
  let rec aux accu n = 
    if ( n = 0 ) 
    then accu
    else aux (a * accu) (pred n)
  in aux 1
(* =============================================================== *)

(* =============================================================== *)
(** Ajouter les valeurs des 8 bits (bool) d'un char dans une Queue *)
(* =============================================================== *)
let bits_of_char q c = 
  let n = ref (int_of_char c) in
  for i = 0 to 7 do
    Queue.push (!n mod 2 = 1) q;
    n := (!n) / 2;
  done
(* =============================================================== *)

(* =============================================================== *)
(** Interface pour la gestion lecture/ecriture bit a bit *)
(* =============================================================== *)
module type IBit =
sig
  type bit_in_channel
  type bit_out_channel
  val bopen_in : string -> bit_in_channel
  val bopen_out : string -> bit_out_channel
  val bread : bit_in_channel -> bool
  val bwrite : bit_out_channel -> bool -> unit
  val bclose_in : bit_in_channel -> unit
  val bclose_out : bit_out_channel -> unit
end
(* =============================================================== *)

(* =============================================================== *)
(** Implementation de la gestion input/output bit a bit *)
(* =============================================================== *)
module Bit:IBit =    
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
    boc.accu <- boc.accu + (if data then 2 ^$ boc.length else 0);
    boc.length <- succ boc.length
     
  (* bflush est privee : utilisee pour terminer l'ecriture
     et vider le buffer meme si on a pas ecrit tous les bits
     du dernier octet *)
  let bflush boc = 
    begin
      output_char boc.oc (char_of_int boc.accu);
      boc.accu <- 0;
      boc.length <- 0
    end
      
  let bclose_out boc = bflush boc; close_out boc.oc
  let bclose_in bic = close_in bic.ic
    
end
(* =============================================================== *)

(* =============================================================== *)
(** Test unitaire pour le module de in.out bit a bit *)
(* =============================================================== *)
let test_unitaire_module_Bit () =
  let file = "test01" in
  let nombre_tests = 200 in
  for i = 0 to nombre_tests do
    let len = 100 + (Random.int 100) in
    let tab = Array.init len (fun _ -> Random.bool ()) in
    begin
      Printf.fprintf stderr "Test d'ecriture lecture avec %d bits : ..." len;
      flush stderr;
      (* ecriture *)
      let boc = Bit.bopen_out file in
      for j = 0 to pred len do
        Bit.bwrite boc tab.(j)
      done;
      Bit.bclose_out boc;
      (* relecture *)
      let bic = Bit.bopen_in file in
      for j = 0 to pred len do
        let br = Bit.bread bic in
        if br <> tab.(j) 
        then 
	  failwith (Printf.sprintf "Erreur : bit %d : Ecrit : %b VS lu : %b\n" j tab.(j) br)
	else ()
      done;   
      Printf.fprintf stderr "...OK\n";
      flush stderr
    end
  done;;
(* =============================================================== *)

(* inscription de la fonction de test dans les tests unitaires *)
TestUnit.add ("Module Bit", test_unitaire_module_Bit)

