(* ====================================================================== *)
(** PROJET HASM 
    Application TEST unitaire de la SUB du circuit HASM 
    Comande : simu -p hasm.bop hasm_in.bop 256 | ./testsub -r 8  *) 
(* ====================================================================== *)

type sens = 
    FAIBLE_vers_FORT
  | FORT_vers_FAIBLE
;;

let val_of_char = function
  | '0' -> 0
  | '1' -> 1
  | c -> failwith (Printf.sprintf "unexpected char : %c ('0' or '1' expected)" c)
;;

(* Extraire la valeur en decimal d'une string en binaire *)
let extract_bin_number sens s =
  let len = String.length s in
  let out = ref 0 in
  match sens with
    | FAIBLE_vers_FORT ->
        begin
          for i = pred len downto 0 do
            out := 2 * (!out) + (val_of_char s.[i])
          done;
          !out
        end
    | FORT_vers_FAIBLE ->
        begin
          for i = 0 downto pred len do
            out := 2 * (!out) + (val_of_char s.[i])
          done;
          !out 
        end
;;

(* separer une chaine en plusieurs valeurs d'apres binaire *)
let split sens len n s = Array.init n (fun i -> extract_bin_number sens (String.sub s (len * i) len));;

let main_test n fct ok notok =
  try
    let sens, len = 
      if Sys.argv.(1) = "-r" 
      then (FAIBLE_vers_FORT, int_of_string Sys.argv.(2))
      else (FORT_vers_FAIBLE, int_of_string Sys.argv.(1))
    in
    while true do
      let s = input_line stdin in
      let res = split sens len n s in
      if fct len res 
      then ok res
      else notok res
    done
  with
    | Invalid_argument _ -> Printf.fprintf stderr "use : %s [-r] len_bits \n" Sys.argv.(0); flush stderr
    | e -> raise e;;

let ( ** ) a =
  let rec aux accu i = 
    if i <= 0 then accu else aux (a * accu) (pred i) 
  in aux 1;;

let mod_positif a b = let r = a mod b in if r < 0 then r + b else r;;

let fct p = function
  | [|a;b;c|] -> (c = mod_positif (a - b) (2 ** p))
  | _ -> failwith "arg error";;
 
let ok = function
  | [|a;b;c|] -> Printf.fprintf stderr "TEST (%d,%d,%d) OK\n" a b c; flush stderr
  | _ -> failwith "arg error";;

let notok = function
  | [|a;b;c|] -> Printf.fprintf stderr "TEST (%d,%d,%d) ERROR\n" a b c; flush stderr; failwith "FIN DU TEST"
  | _ -> failwith "arg error";;

main_test 3 fct ok notok;;
