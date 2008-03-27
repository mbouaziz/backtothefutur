(* ================================================================== *)
(** KISS Is a SubLeq Simulator : simulateur d'execution d'une machine
    SubLeq avec un mode Debug graphique sommaire.
    Permet de generer une famille Ai tel que Slc(Ai) termine en 
    fonction d'une condition c. (à l'aide d'un time out naif) *)
(* ================================================================== *)

(* Utilise la bplib : ~/bebop1.0/boplibs/bplib.cmxa *)
open Graphics
open Bplib
open Bitreader

(* on utilise la class subleq machine en mode debug uniquement *)

let couleurPC = green 
and couleurA = yellow
and couleurB = magenta
and couleurW = red
and couleurC = cyan

;;

class subleq_machine archi init =
object(this)
  val ram = new ram "Machine Subleq" archi archi init
  val length = 2 *$ archi 
  val mutable pc = 0

  method condition_arret = pc = 1
    
  method draw = ram#draw

  method run =
    begin
      while not (this#condition_arret) do
	this#iter
      done;
      prerr_endline "Condition d'arret, fin du calcul"
    end

  method iter = 
    ram#reset_all_color;
    let pc1 = ((pc+1) mod length) 
    and pc2 = ((pc+2) mod length)
    and pc3 = ((pc+3) mod length) in
    let a = ram#read (pc mod length) 
    and b = ram#read pc1 
    and c = ram#read pc2 in
    let a' = ram#read (a mod length)
    and b' = ram#read (b mod length) in
    let diff = (b' - a' + length) mod length in
    ram#set_color pc couleurPC;
    Printf.fprintf stderr "Lire PC\n";
    flush stderr;
    ram#wait;

    ram#set_color a couleurA;
    Printf.fprintf stderr "Allez lire à l'adresse A\n";
    flush stderr;
    ram#wait;

    ram#set_color pc1 couleurPC;
    Printf.fprintf stderr "Lire PC+1\n";
    flush stderr;
    ram#wait;

    ram#set_color b couleurB;
    Printf.fprintf stderr "Allez lire à l'adresse B\n";
    flush stderr;
    ram#wait;

    ram#write b diff;
    ram#set_color b couleurW;
    Printf.fprintf stderr "Ecrire dans %d le resultat de %d - %d = %d\n" b b' a' diff ;
    flush stderr;
    ram#wait;

    let neg = not (diff > 0 && diff < (length / 2)) in
    (
      if neg 
      then 
	begin
	  ram#set_color pc2 green;
	  ram#set_color c couleurC;
	  Printf.fprintf stderr "Ce resultat est negatif ou nul\n";
	  Printf.fprintf stderr "On lit PC2 et on Jump\n";
	end
      else 
	begin
	  ram#set_color pc3 couleurC;
	  Printf.fprintf stderr "Ce resultat est strict positif\n";
          Printf.fprintf stderr "On Jump à PC+3\n";
 	end
    ) ;
    flush stderr;
    ram#wait;

    flush stderr;
    if neg 
    then pc <- c
    else pc <- pc3
end


class quick_subleq archi init =
object(this)
  val ram = Array.init (2 *$ archi) (fun i -> int_of_bits init.(i))
  val length = 2 *$ archi 
  val mutable pc = 0

  method print_ram_asci st = 
	let bm = Array.create_matrix length archi false in
	for i = 0 to pred length do
		bits_of_int bm.(i) ram.(i)
	done;
	BoolMatrix.to_asci_channel bm st

  method print_ram_binary st = 
	let bm = Array.create_matrix length archi false in
	for i = 0 to pred length do
		bits_of_int bm.(i) ram.(i)
	done;
	BoolMatrix.to_bin_channel bm st
	
  method get_ram = ram
  method get_pc = pc

  method condition_arret = pc = 1

  method run =
    begin
      while not (this#condition_arret) do
	this#iter
      done;
      prerr_endline "Condition d'arret, fin du calcul"
    end

  method iter = 
    let pc1 = ((pc+1) mod length) 
    and pc2 = ((pc+2) mod length) in
    let b = ram.(pc1) in
    let a' = ram.(ram.(pc))
    and b' = ram.(b) in
    let diff = (b' - a' + length) mod length in
    ram.(b) <- diff;
    if (diff <= 0) || (diff >= length / 2) 
    then
     pc <- ram.(pc2)
    else
     pc <- ((pc+3) mod length)
end

(* 
   options 
   -g       : graphique pas à pas
   -b + arg : avec une initialisation binaire
   arg      : avec une initialisation asci
*)

type execution =
{
  archi : int;
  mode_graphique : bool;
  init : string option;
  typ : source;
  output_typ : source;
} and source = Binary | Asci


(* ===================================================================== *)
(** Exeption levee quand la ligne de commande est incoherente *)
exception Arguments_Invalides
(* ===================================================================== *)

(* ===================================================================== *)
(** La chaine est-elle un entier ? *)
let is_an_int s = try ignore (int_of_string s);true with Failure _ -> false
(* ===================================================================== *)

(* ===================================================================== *)
(** Deduire d'un tableau Sys.argv le type d'execution
    Lever l'exeption Arguments_Invalides si la lecture echoue *)
(* ===================================================================== *)
let execution_of_argv argv = 
  let n = Array.length argv in
  if n < 2 || n > 5
  then raise Arguments_Invalides
  else
    let typ = ref Asci
    and out = ref Asci
    and dbl = ref false
    and graph = ref false
    and archi = ref None
    and queue_sources = Queue.create () 
    and i = ref 1 in  
    begin
      while (!i < n) do
        begin
          let mot = argv.(!i) in
          match mot with
            (* La chaine vide *)
          | "" -> ()
              (* une option *)
	  | "-g" -> graph := true
          | "-ob" -> out := Binary
	  | "-b" -> 
	      if !dbl then raise Arguments_Invalides 
	      else (dbl := true; typ := Binary)
	  | "-a" -> 
	      if !dbl then raise Arguments_Invalides 
	      else (dbl := true; typ := Asci)
        (* precision de l'archi *)
          | m when (is_an_int m) -> 
              archi := Some (int_of_string m)
          | m -> Queue.add m queue_sources
        end;
        incr i;
      done;
      (* Verification de la coherence *)
      if (!archi = None) || (Queue.is_empty queue_sources) 
      then raise Arguments_Invalides
      else 
	match (!archi, !graph) with
	| (Some a, g) -> 
	    {
	      archi = a;
	      init = Some (Queue.pop queue_sources);
	      mode_graphique = g;
	      typ = !typ;
	      output_typ = !out
	    }
	| _ -> raise Arguments_Invalides
    end 
(* ===================================================================== *)

let _ = 
  prerr_endline "KISS : Kiss Is a Subleq Simulator";
  try
    let exe = execution_of_argv Sys.argv in
    let init_tab = 
      match exe.init with
      | Some file ->
	  if exe.typ = Binary
	  then BoolMatrix.of_bin_file (2 *$ exe.archi) exe.archi file
	  else BoolMatrix.of_asci_file (2 *$ exe.archi) exe.archi file
      | None -> 
	  prerr_endline "Warning, you should give a file to init the Subleq Machine !";
	  [||]
    in
    if exe.mode_graphique 
    then
      let sl = new subleq_machine exe.archi init_tab in 
      begin
	open_graph " 300x400";
	sl#run
      end
    else 
      let sl = new quick_subleq exe.archi init_tab in
      begin
	sl#run;
        if exe.output_typ = Asci 
	then sl#print_ram_asci stdout
	else sl#print_ram_binary stdout
      end
  with
  | Arguments_Invalides -> 
      prerr_endline (Printf.sprintf "use : %s [-g] archit.(int) [-b/-a] file [-ob] > output" Sys.argv.(0))
  
