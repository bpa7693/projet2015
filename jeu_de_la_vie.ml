
(************************************************************)
(*                     Jeu de la vie                        *)
(************************************************************)

#load "graphics.cma";;
open Graphics;;


(* Définitions de base du jeu de la vie *)

(* Couleurs de cellule possibles *)

let couleur_cellule = function
  | 0 -> white
  | _ -> black ;;
  
(* Cellule vivante *)

let nouvelle_cellule = 1 ;;

(* Cellule morte *)

let vide = 0 ;;

(* Taille en pixels d'une cellule *)

let size_cellule = 10 ;;

(* Etat de vie d'une cellule. Renvoie VRAI si vivante, FAUX sinon. *)

let est_vivante cellule = cellule <> vide ;;

(* Retourne le nouvel état d'une cellule à partir de celle-ci 
 * et de ses voisines. *)
 
let regles cellule voisine = 
  if (cellule = vide) && (voisine = 3) then 
    nouvelle_cellule
  else 
    if (voisine = 3) || (voisine = 2) then 
      cellule
    else 
      vide ;;


(************************************************************)
(*                     Préparation                          *)
(************************************************************)

(************   Listes  *************)

(* Le plateau sera représenté par une liste de listes d’entiers 
 * (appelée matrice ici *)

(* Retourne une matrice carré de taille n x n remplie 
 * de la valeur en x. *) 

let gen_plateau taille x =
  let rec gen_ligne valeur = function
    | 0 -> []
    | n -> valeur :: gen_ligne valeur (n-1)
  in gen_ligne (gen_ligne x taille) taille;;


(*** Retourne une valeur en (x, y) dans la matrice plateau. ***)
(* failwith remplacée par vide si "out of bounds" *)

let get_cellule (x,y) plateau =
  let rec extract_ligne = function
    | (_, []) -> vide
    | (1, e::_) -> extract_cellule (y,e)
    | (n, _::l) -> extract_ligne ((n-1), l)
  and extract_cellule = function 
    | (_, []) -> vide
    | (1, e::l) -> e
    | (n, _::l) -> extract_cellule ((n-1), l)
  in extract_ligne (x, plateau);;

(* Remplace la valeur en (x,y) dans la matrice plateau par la valeur cellule *)

let put_cellule cellule (x,y) plateau =
  let rec process_ligne = function
    | (_, []) -> []
    | (1, e::l) -> (process_colonne (y,e)) :: l
    | (n, e::l) -> e :: (process_ligne ((n-1), l))
  and process_colonne = function
    | (_, []) -> []
    | (1, e::l) -> cellule :: l
    | (n, e::l) -> e :: (process_colonne ((n-1), l))
  in process_ligne (x, plateau);;

(*** Retourne le nombre de cellules vivantes autour de 
	 la cellule en (x, y) dans plateau. ***)

let count_voisines (x,y) plateau = 
  let add (x,y) = 
    if est_vivante (get_cellule (x,y) plateau) then
      1
    else
      0
  in
      add (x-1,y-1) + add (x-1,y) + add (x-1,y+1)
    + add (x,y-1) + add (x,y+1)
    + add (x+1,y-1) + add (x+1,y) + add (x+1,y+1) ;;

 
(************************************************************)
(*                  Partie Graphique                        *)
(************************************************************)

(* Ouvre une fenetre de dimension size x size *)

let open_window size = open_graph(string_of_int size ^ "x" ^ string_of_int (size+20));;

(* Définition de la couleur grise *)

let grey = rgb 127 127 127 ;;

(* Dessine un carré gris de coin gauche à (x,y) de taille size et de couleur color *)

let draw_fill_square (x,y) size color = 
  set_color color; fill_rect x y size size;
  set_color grey; draw_rect x y size size;;

(* Dessine la cellule de coordonnées (x, y), de taille size et de couleur c *)
  
let draw_cellule (x,y) size c =  
  draw_fill_square (x * size, y * size) size c ;;

(* Dessine un plateau sur la fenêtre graphique prenant en paramètre :
	- La matrice plateau, représentative du plateau de jeu. 
	- La taille size, en pixels, des cellules. *)
(* Le point de départ est à (1,1) pour éviter de coller au cadre. *)

let draw_plateau plateau size =
  let rec iter_ligne x = function
    | [] -> ()
    | e::l -> iter_colonne x 1 e; iter_ligne (x+1) l
  and iter_colonne x y = function
    | [] -> ()
    | e::l -> draw_cellule(x,y) size (cellule_color e); iter_colonne x (y+1) l
  in 
  clear_graph () ;      
  iter_ligne 1 plateau       
;;

(************************************************************)
(*                   Le jeu                                 *)
(************************************************************)

(* place count new cellules randomly in plateau (size is the plateau size!) *)

let seed_life plateau size count = 
  let rec plant plateau = function
    | 0 -> plateau
    | n -> plant (put_cellule nouvelle_cellle 
			(1 + Random.int size, 1+ Random.int size) plateau) (n-1)
  in plant plateau count ;;


let new_plateau size n = seed_life (gen_plateau size vide) size n ;;

let next_generation plateau = 
  let rec map_ligne x = function
    | [] -> []
    | e::l -> map_colonne x 1 e :: map_ligne (x+1) l
  and map_colonne x y = function
    | [] -> []
    | e::l -> regles e (count_voisines (x,y) plateau) :: map_colonne x (y+1) l
  in 
  map_ligne 1 plateau ;;

let rec game plateau = function
  | 0 -> ()
  | n -> draw_plateau plateau size_cellule ; game (next_generation plateau) (n-1) ;;


let new_game size nb = 
  open_window (size*size_cellule + 40) ;
  game (new_plateau size (size*size_cellule)) nb ;;

new_game 50 200 ;;
