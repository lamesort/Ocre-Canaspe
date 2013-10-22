(* Dimensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, 
   (Sdlvideo.surface_info img).Sdlvideo.h)


(* Fonction pour ajouter un element dans une liste pointee*)
let add_list elt l = l := elt::!l;;

(* On remplie la liste de taille la hauteur de l'image et contenant le *)
(* nombre de pixel noirs par ligne  *) 
let fill_black_lines img = let (w,h) = get_dims img in
			   let r = ref 0 in
			   let lines_with_black = ref  [] in
     for i=h downto 0 do
       for j=0 to w do
         if (Sdlvideo.get_pixel_color img j i = (0,0,0)) then
            r := !r + 1;
       done ;
        add_list !r lines_with_black;
        r := 0;
     done;
	!lines_with_black;;


(* On remplie la liste de taille la largeur de l'image et contenant le *)
(* nombre de pixel noirs par colonne  *) 
let fill_black_columns img = let (w,h) = get_dims img in
			   let r = ref 0 in
			   let columns_with_black = ref  [] in
     for j=w  downto 0 do
       for i=0 to h do
         if (Sdlvideo.get_pixel_color img  j i= (0,0,0)) then
            r := !r + 1;
       done ;
        add_list !r columns_with_black;
        r := 0;
     done;
	!columns_with_black;;



(* Fonction de debug pour tester le contenu des listes*)
let print_reflist img = 
     for i=0 to ((Sdlvideo.surface_info img).Sdlvideo.h) - 1 do
     begin
     print_int (List.nth (fill_black_lines img)  i) ;      
     print_string ("\n");
     end ;
        done;;

		
		
		
(* Position de la premiere colonne et la derniere colonne qui ont au moins *)
(* un pixel noir pour pouvoir encadrer le texte *)
let first_columns_pos img = let r = ref 0 in
			    let columns = fill_black_columns img in
              while List.nth (columns)  !r < 2 do
              r:= !r + 1;
	      done ;
              !r;;



let last_columns_pos img = let (w,h) = get_dims img in
                           let r = ref (w-1)  in
                           let columns = fill_black_columns img in
              while List.nth (columns)  !r < 3 do
              r:= !r - 1;
              done ;
              !r;;





(* Position de la premiere ligne et la derniere ligne qui ont au moins *)
(* un pixel noir pour pouvoir encadrer le texte *)
let first_line_pos img = let r = ref 0 in
                         let lines = fill_black_lines img in
              while List.nth (lines)  !r < 2 do
              r:= !r + 1;
              done ;
              !r;;



let last_line_pos img = let (w,h) = get_dims img in
                        let r = ref (h-1) in
                        let lines = fill_black_lines img in
              while List.nth (lines)  !r < 2 do
              r:= !r - 1;
              done ;
              !r;;



(* On encadre la zone de texte *)
let draw_where_text_is img = let coin =  first_columns_pos img and
                                 cout =  last_columns_pos img and
                                 liin = first_line_pos img and
                                 liout = last_line_pos img in
    for i= liin - 2 to liout +2  do
     for j= coin  -2 to coin  -1 do
       Sdlvideo.put_pixel_color img j i  (255,0,0)
     done ;
       for j= cout  +1 to cout +2 do
	 Sdlvideo.put_pixel_color img j i  (255,0,0)
       done
    done ;
   for j = coin to cout do
     for i=liin -2 to liin -1  do
       Sdlvideo.put_pixel_color img j i  (255,0,0)
     done ;
     for i=liout +1 to liout  +2  do
       Sdlvideo.put_pixel_color img j i  (255,0,0)
     done ;
   done ;; 

   
   
   
   
   
   
   
(* On Mesure la taille du rectangle pour estimer la taille des lignes/colonnes *)
   let rectangle_height img = last_line_pos img - first_line_pos img ;;

    let  rectangle_width img = last_columns_pos img - first_columns_pos img;;

	

	(* Detection des lignes *)
	
	
(* Le ouexclusif *)
let ouexc (a ,b) = if a then not b else b;;

(* On parcours l'image et dès que  *)
    let fill_blocks_list img = let blocks_list = ref [] in
                               let r = ref 1 in
			       let fbl = fill_black_lines img in
       for i = first_line_pos img to rectangle_height img do
           if ouexc (List.nth (fbl) i = 0 ,List.nth (fbl) (i-1) <> 0)  then 
        add_list (i,r) blocks_list ; r:= !r + 1 
       done ;
     !blocks_list

(* Fonction qui remplie la ligne x de l'image pour la mettre en evidence *)
    let draw_line img x = 
      for i =  first_columns_pos img to  last_columns_pos img do
         Sdlvideo.put_pixel_color img i x (255,0,255)
      done;;   

    let draw_columns img x y = 
      for i = x to y  do
         Sdlvideo.put_pixel_color img x i (0,255,0)
      done;;




(* Fonction qui dessine et detecte les lignes *)
(* A chaque fois que l'on change de zone *)
(* ( on passe d'une ligne vide a une non vide) *)
(* on dessine une ligne *)
    let fill_lines img = let fbl = fill_black_lines img in
			 let lines = ref [] in
      for i=  first_line_pos img to last_line_pos  img do
	if (List.nth  (fbl) i) = 0 && 
        ((List.nth  (fbl) (i-1)) <> 0 || 
        (List.nth  (fbl) (i+1)) <> 0  ) then 
          draw_line img i ; add_list i lines 
      done;
!lines;;


    let draw_carac img = let fl = fill_lines img in
			 let r = ref 0 in
			 for i=  first_line_pos img to last_line_pos  img do
			   if List.nth  fl !r = i then
			     begin
                        (*      for j = first_columns_pos img to last_columns_pos img do
				 if Sdlvideo.get_pixel_color img  j (List.nth  fl (!r +1) - 1 ) = (255,255,255) then    
				   draw_columns img i  (List.nth  fl (!r +1) - 1 )
			      done;
			       r := !r + 1; *)
			      
			     end;
			 done;
			 draw_where_text_is img;;


    

