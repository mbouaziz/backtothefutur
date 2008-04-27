(* =================================================================== *)
(** BopWay
    @author Mathieu Barbin, Mehdi Bouaziz	*)
(* =================================================================== *)

(* #load "unix.cma";; *)

exception SigInt;;

(* Donner une chaine a partir d'une adresse de socket *)
let string_of_sockaddr = function
  | Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET (inet, i) -> 
      Printf.sprintf "(%s,%d)" (Unix.string_of_inet_addr inet) i;;

(* Determine si le domaine est unix, ou internet, 
   en lisant l'adresse *)
let domain_of = function
  | Unix.ADDR_UNIX _ -> Unix.PF_UNIX
  | _ -> Unix.PF_INET;;

(* Etablir un server generique parametre par une fonction server_fun *)
(* La fonction tourne en boucle *)
let establish_server sockaddr =
	Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _->raise SigInt));
	let domain = domain_of sockaddr in
	let sock = Unix.socket domain Unix.SOCK_STREAM 0 
	in Unix.bind sock sockaddr ; (* lier la socket a l'adresse *)
	Unix.listen sock 30; (* accepte 30 requete en meme temps au max *)
	let (sserver, callerserver) = Unix.accept sock in (* attend un nouveau serveur *)
	Printf.printf "\nReception of server from %s\n" (string_of_sockaddr callerserver); flush stdout;
	let inchanserver = Unix.in_channel_of_descr sserver
  	and outchanserver = Unix.out_channel_of_descr sserver
	in
	try 
		while true do
			let (sclient, callerclient) = Unix.accept sock in (* attend un nouveau client *)
			Printf.printf "\nReception of client from %s\n" (string_of_sockaddr callerclient); flush stdout;
			let inchanclient = Unix.in_channel_of_descr sclient 
			and outchanclient = Unix.out_channel_of_descr sclient 
			in
			try
				while true do
					output_string outchanserver ((input_line inchanclient)^"\n");
					flush outchanserver;
					output_string outchanclient ((input_line inchanserver)^"\n");
					flush outchanclient;
				done;
			with
				| End_of_file -> (
					try close_in inchanclient;
						close_out outchanclient;
					with _ -> () )
			done
	with
		| SigInt -> ( try close_in inchanserver;
						close_out outchanserver;
					with _ -> () )
;;

(* Optenir l'adresse internet locale *)
let get_my_addr () =
  (Unix.gethostbyname(Unix.gethostname())).Unix.h_addr_list.(0) ;;

(* Lancer le server *)
let main_server () =
  if Array.length Sys.argv < 2 
  then (Printf.eprintf "use : %s port\n" Sys.argv.(0); exit 1)
  else 
    try
      let port = int_of_string Sys.argv.(1) in 
      let mon_adresse = get_my_addr() 
      in 
      begin
	Printf.fprintf stderr "Lancement du server : %s, port :%d\n"
	 (Unix.string_of_inet_addr mon_adresse) port; 
	flush stderr;
	establish_server (Unix.ADDR_INET(mon_adresse, port))
      end
  with
    Failure("int_of_string") -> 
      Printf.eprintf "%s : mauvais numero de port\n" Sys.argv.(0) ;;

let _ = Unix.handle_unix_error main_server ();;
