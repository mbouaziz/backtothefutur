(* ================================================================== *)
(** KISS Is a SubLeq Simulator : simulateur d'execution d'une machine
    SubLeq avec un mode Debug graphique sommaire *)
(* ================================================================== *)

#load "graphics.cma";;
open Graphics;;

open_graph " 900x600";;
close_graph ();;
let clean_up () = 
  set_color black;
  fill_rect 0 0 900 600;;

clean_up ();;

set_window_title "Kiss : Debug Mode";;

let digits_of_int nd i =
  let s = String.make nd '0' in
  let s2 = string_of_int i in
  let len2 = String.length s2 in
  for i = 0 to pred (min nd len2)  do
    s.[nd-1-i] <- s2.[len2-1-i]
  done;
  s;;

let center imin imax j mot = 
  let (di, dj) = text_size mot in
  let add = imin + ((imax-imin-di)/2) in
  let x,y = current_point () in
  moveto add j;
  draw_string mot;
  moveto x y;;
  

type style_boite = 
{
  fond_color : color;
  num_digit : int;
  text_color : color;
  plateau_color : color
};;

type info_boite = 
{
  box_name : string;
  coord : int * int;
  style : style_boite
}
;;  

(** l'interface des classes box d'affichage *)
class type type_box = 
object
  val ib : info_boite
  val mutable v : int
  method set : int -> unit
  method private draw : unit
  method init : unit
end

;;

class box info : type_box = 
object(this)
  val ib = info
  val mutable v = 0
  method private draw = 
    begin
      (* on efface *)
      set_color ib.style.fond_color;
      match ib.coord, ib.style.num_digit with
      | (i,j), d -> 
	  begin
	    fill_rect i j ((d+2)*6) 13; 
	    (* on reecrit le cadre *)
	    set_color ib.style.plateau_color;
	    draw_poly_line [|(i,j+13); (i,j); (i+(d+2)*6, j); (i+(d+2)*6, j+13)|];
	    (* on ecrit la valeur *)
	    set_color ib.style.text_color;
	    moveto (i+6) (j+2);
	    let vs = digits_of_int ib.style.num_digit v in
	    draw_string vs;
	    (* on ecrit le nom de la boite en dessous *)
	    center i (i+(d+2)*6) (j-15) ib.box_name
	  end
    end

  method set x = 
    if v = x 
    then ()
    else (v <- x;this#draw) 
    
  method init =
    v <- 0;
    this#draw
end;;


let style1 = 
{ 
  fond_color = black;
  num_digit = 4;
  text_color = red;
  plateau_color = yellow
};;

let ibPC = 
{
  box_name = "pc";
  coord = 180 , 580;
  style = style1
};;


let ibA = 
{
  box_name = "A";
  coord = 30 , 580;
  style = style1
};;

let ibB =
{
  box_name = "B";
  coord = 130 , 580;
  style = style1
};;

let ibC =
{
  box_name = "C";
  coord = 230 , 580;
  style = style1
};;

let ibvA = 
{
  box_name = "*A";
  coord = 30 , 550;
  style = style1
};;

let ibvB =
{
  box_name = "*B";
  coord = 130 , 550;
  style = style1
};;

let ibvBA =
{
  box_name = "*B - *A";
  coord = 230 , 550;
  style = style1
};;


let boxA = new box ibA;;
let boxB = new box ibB;;
let boxC = new box ibC;;
let boxvA = new box ibvA;;
let boxvB = new box ibvB;;
let boxvBA = new box ibvBA;;

clean_up ();;
boxA#init;;
boxB#init;;
boxC#init;;
boxvA#init;;
boxvB#init;;
boxvBA#init;;


type info_dump =
{
  style_d : style_boite;
  largeur_col : int;
  largeur_ligne : int;
  num_col : int;
  num_ligne : int;
  debut : int * int;
  cache : int * int * int * int
}

(** l'interface de la classe code_dump *)
class type type_code_dump = 
object
  val code : int array
  val mutable pc : int (* program counter *)
  val mutable adressA : int
  val mutable adressB : int
  val mutable adressC : int
  val mutable offset : int  (* l'indice de la premiere data affichee *)
  method set_value : int -> int -> unit
  method set_offset : int -> unit
  method draw : unit
end


class code_dump init fond ligne_color cx cy dcx dcy x y largeur_col hauteur_col 
  num_col num_lign largeur_lign = 
object(this)
  val code = Array.copy init
  val mutable pc = 0
  val mutable adressA = 0
  val mutable adressB = 0
  val mutable adressC = 0
  val mutable offset = 0
  method set_value index (v:int) = code.(index) <- v
  method set_offset no = offset <- no; this#draw
    
  method clean_up =
    set_color fond;
    fill_rect cx cy dcx dcy;
    

  method draw = 
    this#clean_up;
    set_color ligne_color;
    for i = 0 to pred num_col do  
      draw_rect (x+i*largeur_col) y largeur_col hauteur_col 
    done;  
end

let cd = new code_dump (Array.init 500 (fun i -> i)) blue yellow 0 0 900 520
50 10 100 500 8 30 15;;

clean_up();;
cd#clean_up;;
cd#draw;;
