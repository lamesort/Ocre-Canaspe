(* main *)
let main () =
  begin
    (* Nous voulons 1 argument *)
    if Array.length (Sys.argv) < 2 then
      failwith "Il manque le nom du fichier!";
    (* Initialisation de SDL *)
    Basic_tools.sdl_init ();
    (* Chargement d'une image *)
    let img = Sdlloader.load_image Sys.argv.(1) in
    (* On r�cup�re les dimensions *)
    let (w,h) = Basic_tools.get_dims img in
    (* On cr�e la surface d'affichage en doublebuffering *)
    let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
      (* on affiche l'image *)
      Basic_tools.show img display;






    (* on attend une touche avant de passer en niveaux de gris *) 
    Basic_tools.wait_key ();
    let newImg = Filters.img2grey img in
    Basic_tools.show newImg display;

    (* on attend une touche avant de passer en noir & blanc *) 
    Basic_tools.wait_key ();
    let matr1 = Filters.binarize newImg (Filters.colormoy newImg) in
    let newImg1 = Matrix.matr2img matr1 in
    Basic_tools.show newImg1 display;

    (* on attend une touche avant d'�liminer le bruit *) 
    Basic_tools.wait_key();
    let matr2 = Filters.median matr1 in
    let newImg2 = Matrix.matr2img matr2 in
    Basic_tools.show newImg2 display;
    
    (* on attend une touche d'appliquer un flou gaussien 
       avc la matrice de convolution *)
    Basic_tools.wait_key ();
    let newImg3 = Filters.convolution Filters.kernel_gaussian newImg2 in
    Basic_tools.show newImg3 display;

    Basic_tools.wait_key ();
    Caracrecogn.draw_carac newImg1; 
    Basic_tools.show newImg1 display;
  
    Basic_tools.wait_key ();
    exit 0;
  end
 
let _ = main ()
