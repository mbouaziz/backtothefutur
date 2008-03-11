(* ============================================================== *)
(** TESTS UNITAIRES : Projet SubLeq 
    Ce module contient une reference liste de couple 
    string * fonction de type unit -> unit
    Qui sont les fonctions a tester lors des tests unitaires.

    Dans chaque module, on inscrit une telle fonction avec 
    la fonction add du module TestUnit, avec une breve description
    de ce qu'on teste.

    On cree un executable qui s'appelle ./testunit et qui depend
    de tous les fichiers sources, qui lance la methode run *)
(* ============================================================== *)

module TestUnit = 
struct
  let todo = ref []

  let add ((descriptif, (fct : unit -> unit) )as t) = todo := t::(!todo)

  let run () =
    let index = ref 1 in
    let rec aux = function
      | [] -> () 
      | (s, fct)::q -> 
	  begin
	    Printf.fprintf stderr "=====\n";
	    Printf.fprintf stderr "\nTEST %d : %s\n\n" (!index) s;
	    flush stderr;
	    (
	      try
		fct ()
	      with
	      | e -> 
		  Printf.fprintf stderr "ERREUR SURVENUE, test : \"%s\"\n" s;
		  raise e;
	    );
	    flush stderr;
	    incr(index)
	  end
    in 
    begin
      Printf.fprintf stderr "============= ~~ TESTS UNITAIRES ~~ ============\n";
      flush stderr;
      aux (List.rev (!todo));
      Printf.fprintf stderr "=============  ~~ FIN DES TESTS ~~  ============\n\n";
      Printf.fprintf stderr "TestUnit : Aucune erreur rencontree.\n\n";
      flush stderr
    end 
end
