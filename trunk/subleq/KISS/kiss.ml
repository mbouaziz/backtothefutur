(* ================================================================== *)
(** KISS Is a SubLeq Simulator : simulateur d'execution d'une machine
    SubLeq avec un mode Debug graphique sommaire.
    Permet de generer une famille Ai tel que Slc(Ai) termine en 
    fonction d'une condition c. (à l'aide d'un time out naif) *)
(* ================================================================== *)

(* Utilise la bplib : ~/bebop1.0/boplibs/bplib.cmxa *)
(* doc en ligne : http://www.eleves.ens.fr/home/barbin/bebop/bplib/Bplib.BoolArray.html *)
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
  val length = 2 $$ archi 
  val mutable pc = 0

  method condition_arret = pc = 1
    
  method draw = ram#draw

  method run =
    begin
      prerr_endline "Graphic Mode : run...";
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


(* 
   Detection des cycles dans l'execution Subleq, voir le principe utilise dans le rho de pollard :
   - algorithme de Floyd : http://fr.wikipedia.org/wiki/Algorithme_du_lièvre_et_de_la_tortue 
*)

class quick_subleq archi = 
object(this)
  (* on utilise 2 ram, pour implémenter l'algorithme de Floyd de détection de cycle *)
  (* on conserve l'etat initial pour calculer le ratio de change *)
  val init = Array.make_matrix (2 $$ archi) archi false
  val ram = Array.init (2 $$ archi) (fun _ -> 0)
  val mutable pc = 0 
  val length = 2 $$ archi 
    
  val ram2 = Array.init (2 $$ archi) (fun _ -> 0) 
  val mutable pc_ram2 = 0

  (* detection d'un cycle *)
  val mutable cycle = false
    
  (* un compteur d'iteration *)
  val mutable index = 0

  (* tester l'egalite des 2 rams *)
  (* le pc doit etre aussi pris en compte *)
  method equal =
    if pc <> pc_ram2
    then false
    else
      let rec aux i = 
        if i >= length then true (* on a parcouru toutes les cases sans tomber sur des distinctes *)
        else 
          if (Array.unsafe_get ram i) <> (Array.unsafe_get ram2 i) then false
          else aux (succ i)
      in aux 0

  (* calculer a tout instant le pourcentage de difference bits / bits entre la ram, et l'init *)
  method ratio_change =
    let ch = ref 0 and bits = Array.make archi false in
    for i = 0 to pred length do
      BoolArray.load_of_int bits ram.(i); (* load la valeur ram.(i) dans bits *)
      for j = 0 to pred archi do
        if init.(i).(j) <> bits.(j)
        then 
          begin
            (* 
               Printf.fprintf stderr "DIFF (%d,%d) : +1 / %d\n" i j (!ch);
               flush stderr;
            *)
            incr(ch)
          end
        else ()
      done;
    done;
    (* le nombre total de bits est length * archi *)
    (float_of_int (100 * !ch)) /. (float_of_int (length * archi))

  (** reloader une nouvelle configuration dans la machine : plus simplement a partir d'un tableau d'entier modulo length *)
  method load value =
    let len_value = Array.length value in
    if (len_value <> length) || ((Array.length value.(0)) <> archi)
    then 
      begin
        Printf.fprintf stderr "Load : mauvais tableau d'import\n";
        flush stderr
      end
    else 
      begin
        pc <- 0;
        pc_ram2 <- 0;
        index <- 0;
        cycle <- false;
        (* l'init, la ram 1 et la ram 2 *)
        for i = 0 to pred length do
          for j = 0 to pred archi do
            init.(i).(j) <- value.(i).(j)
          done;
          let r = BoolArray.to_int value.(i) in
          ram.(i) <- r;
          ram2.(i) <- r
        done
      end

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
	
  method condition_arret = pc = 1

  method stop cycle =
    if cycle 
    then prerr_endline "[ ;-( ] Cycle d'execution : Fin du calcul sur 'etat d'abandon du calcul'."
    else 
      let ratio = this#ratio_change in
      begin
        Printf.fprintf stderr "[ ;-) ] Etape %d : Condition d'arret. Fin du calcul : IM %2.2f %c diff\n" index ratio '%';
        flush stderr
      end 
          
  method run =
    begin
      prerr_endline "Quick Mode : run...";
      while (not (this#condition_arret)) && (not(cycle)) do
	this#iter;
        let cmp = this#equal in
        if cmp
        then cycle <- true
        else ()
      done;
      if cycle
      then
        begin
          (* on veut etre sur que la situation d'arret n'est pas rencontrée entre m et 2m *)
          let stop = 2 * index in
          while (not (this#condition_arret)) && (index <= stop) do
            this#iter_one_only
          done;
          if index > stop
          then (this#stop true; true) 
          else (this#stop false; false) 
        end
      else (this#stop false; false)
    end

  (* une fois qu'on a fm(r) = f2m(r), on itere de m à 2m pour s'assurer 
     que la condition d'arret n'est pas rencontrée *)
  method iter_one_only =
    begin
      index <- succ index;
      (* 1 itération sur la ram 1 *)
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
        pc <- ((pc+3) mod length); 
    end

  method iter =
    begin
      index <- succ index;
      (* 1 itération sur la ram 1 *)
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
        pc <- ((pc+3) mod length);
      (* 2 itérations sur la ram 2 *)
      for i = 0 to 1 do
        let pc1 = ((pc_ram2+1) mod length) 
        and pc2 = ((pc_ram2+2) mod length) in 
        let b = ram2.(pc1) in
        let a' = ram2.(ram2.(pc_ram2))
        and b' = ram2.(b) in
        let diff = (b' - a' + length) mod length in
        ram2.(b) <- diff;
        if (diff <= 0) || (diff >= length / 2) 
        then
          pc_ram2 <- ram2.(pc2)
        else
          pc_ram2 <- ((pc_ram2+3) mod length)
      done
    end
   
end

type execution =
{
  archi : int;
  mode_graphique : bool;
  init : string option;
  typ : source;
  output_typ : source;
} and source = Binary | Asci

let int_d d i = 
  let j = string_of_int i in
  let len = String.length j in
  if len >= d
  then String.sub j (len - d) d
  else Printf.sprintf "%s%s" (String.make (d - len) '0') j;;

(* ===================================================================== *)
(** Classe pour générer une famille de programmes qui terminent *)
(* ===================================================================== *)
class subleq_generator archi num_chiffres prefixe_antecedent prefixe_image output_typ =
  let length = 2 $$ archi in
object(this)
  val q_subleq = new quick_subleq archi
  val antecedent = Array.make_matrix length archi false
  val mutable index = 0

  (** modifier antecedent par effet de bord *)
  method random_generate = 
    for i = 0 to pred length do
      let r = Random.int length in
      BoolArray.load_of_int antecedent.(i) r
    done

  (** generer 1 couple en incrementant l'indice *)
  method generate_one =
    begin
      let cycle = ref true in
      while (!cycle) do
        (* Generer un code aleatoire *)
        this#random_generate;
        (* Le loader dans la quick_subleq *)
        q_subleq#load antecedent;
        (* Lancer la machine et recuperer l'etat final : y a t'il un cycle ? *)
        cycle := q_subleq#run
      done;
      index <- index + 1;
      (* On est sorti : il n'y a pas de cycle *)
      (* On va imprimer la source et l'image *)
      let index_string = int_d num_chiffres index in
      let sourcefile = Printf.sprintf "%s%s" prefixe_antecedent index_string
      and imagefile = Printf.sprintf "%s%s" prefixe_image index_string in
      let source_oc = open_out sourcefile and image_oc = open_out imagefile in
      (
        match output_typ with
          | Asci -> 
              begin
                Printf.fprintf stderr "[ %s --> ] Sauvegarde asci de l'antecedent RAM : %s\n" index_string sourcefile;
                flush stderr;
                BoolMatrix.to_asci_channel antecedent source_oc;
                flush source_oc;
                
                Printf.fprintf stderr "[ %s --> ] Sauvegarde asci de l'image exec RAM : %s\n" index_string imagefile;
                flush stderr;
                q_subleq#print_ram_asci image_oc;
                flush image_oc
              end
          | Binary -> 
              begin
                Printf.fprintf stderr "[ %s --> ] Sauvegarde binaire de l'antecedent RAM : %s\n" index_string sourcefile;
                flush stderr;
                BoolMatrix.to_bin_channel antecedent source_oc;
                flush source_oc;
                
                Printf.fprintf stderr "[ %s --> ] Sauvegarde binaire de l'image exec RAM : %s\n" index_string imagefile;
                flush stderr;
                q_subleq#print_ram_binary image_oc;
                flush image_oc  
              end
      );
      close_out source_oc;
      close_out image_oc
    end
    
end 



(* 
   options 
   -g       : graphique pas à pas
   -b + arg : avec une initialisation binaire
   arg      : avec une initialisation asci
   -s       : generation
*)



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

(* =================================================================== *)
(** Pratique : une fonction IO assez generique *)
(* =================================================================== *)
let get_something message message_error filtre traitement =
  let rec aux () = 
    Printf.fprintf stdout "%s" message;
    flush stdout;
    let s = input_line stdin in
    if filtre s 
    then traitement s
    else (Printf.fprintf stdout "[!] %s\n" message_error ; aux ())
  in aux ();;
(* =================================================================== *)

let ident_ok s = 
  let len = String.length s in
  if len <= 0 then false
  else
    let cA = int_of_char 'A' and cZ = int_of_char 'Z' and ca = int_of_char 'a' and cz = int_of_char 'z' in
    let rec aux i =
      if i >= len then true 
      else
        let c = int_of_char s.[i] in
        if ((c >= cA) && (c <= cZ)) || ((c >= ca) && (c <= cz)) || (s.[i] = '_') 
        then aux (succ i)
        else false
    in aux 0;;


let _ = 
  prerr_endline "KISS : Kiss Is a Subleq Simulator";
  Random.self_init ();
  try
    if List.mem "-s" (Array.to_list Sys.argv)
    then
      begin
        (* on est en mode generation *)
        let archi = get_something 
          "> Quelle est l'architecture de la machine ? (nombre de bits) : " "Entier > 0 attendu" 
          (fun i -> (is_an_int i)&&((int_of_string i)>0)) int_of_string 
        and prefixe_ant = get_something 
          "> Quel est le prefixe choisi pour les programmes subleq ?    : "
          "chaine (A-Z | a-z | _)+ attendue" ident_ok (fun s -> s)
        in 
        let prefixe_im = get_something 
          "> Quel est le prefixe choisi pour les images subleq ?        : "
          (Printf.sprintf "chaine (A-Z | a-z | _)+ attendue <> %s" prefixe_ant) 
          (fun s -> (ident_ok s)&&(s<>prefixe_ant)) (fun s -> s)
        and number = get_something 
          "> Quel est le nombre de programmes souhaités pour la série ? : "
          "nombre > 0 attendu" (fun s -> (is_an_int s)&&(int_of_string s > 0)) int_of_string 
        and typ = get_something 
          "> Quel est le format de sauvegarde ? (b : binaire, a : asci) : "
          "a / b attendu" (fun s -> s = "a" || s = "b") (fun s -> if s = "a" then Asci else Binary)
        in
        let subleq_gen = new subleq_generator archi (String.length (string_of_int number)) prefixe_ant prefixe_im typ in
        begin
          for i = 0 to pred number do
            subleq_gen#generate_one
          done
        end
      end
    else 
      let exe = execution_of_argv Sys.argv in
      let init_tab = 
        match exe.init with
          | Some file ->
	      if exe.typ = Binary
	      then BoolMatrix.of_bin_file (2 $$ exe.archi) exe.archi file
	      else BoolMatrix.of_asci_file (2 $$ exe.archi) exe.archi file
          | None -> 
	      prerr_endline "Attention, vous devriez donner un fichier pour initialiser la Subleq-Machine !";
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
        let sl = new quick_subleq exe.archi in
        begin
          sl#load init_tab;
	  ignore(sl#run);
          if exe.output_typ = Asci 
	  then sl#print_ram_asci stdout
	  else sl#print_ram_binary stdout
        end
  with
    | Arguments_Invalides -> 
        prerr_endline (Printf.sprintf "executing use   : %s [-g] archit.(int) [-b/-a] file [-ob] > output" Sys.argv.(0));
        prerr_endline (Printf.sprintf "genrerating use : %s -s" Sys.argv.(0)) 
        
