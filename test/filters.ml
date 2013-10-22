(* Intensité lumineuse du pixel (r,g,b) compris entre 0 et 1 *)
let intensitylevel (r,g,b) = 
  ( 0.3 *. float_of_int(r) 
   +. 0.59 *. (float_of_int)g 
   +. 0.11 *. (float_of_int)b ) /.255.;;

(* A partir d'un triplet (r,g,b), renvoie son équivalent gris *)
let color2grey (r,g,b) = 
  let a = int_of_float (intensitylevel(r,g,b) *. 255.) 
  in (a, a, a);;

(* Rend l'image img en nuances de gris dans grey_img *)
let img2grey img =
  begin
    let (w,h) = Basic_tools.get_dims(img) in
    let grey_img = (Sdlvideo.create_RGB_surface_format img [] w h) in
    for y=0 to (h-1) do
      for x=0 to (w-1) do
	let g = Basic_tools.onlyone (color2grey(Sdlvideo.get_pixel_color img x y)) in
	Sdlvideo.put_pixel_color grey_img x y (g,g,g);
      done;
    done;
    grey_img
  end

(* Moyenne des nuances de gris *)
let colormoy grey_img =
  let moy = ref 0 in
  let (w,h) = Basic_tools.get_dims(grey_img) in
  for y=0 to (h-1) do
    for x=0 to (w-1) do
      moy := !moy + (Basic_tools.onlyone (Sdlvideo.get_pixel_color grey_img x y));
    done;
  done;
    !moy / (w*h);;

(* Surface & moyenne des nuances de gris
   -> Matrice Noir & blanc *)
let binarize grey_img moy =
  begin 
    let (w,h) = Basic_tools.get_dims(grey_img) in
    let matr = Matrix.make w h 0 in
    for y=0 to (h-1) do
      for x=0 to (w-1) do
	let g = Basic_tools.onlyone (Sdlvideo.get_pixel_color grey_img x y) in
	if g > moy then
	  matr.(x).(y) <- 1
	else
          matr.(x).(y) <- 0;
      done;
    done;
    matr
  end

(* Elimination du bruit par la médiane:
   Matrice de 0&1 -> Matrice de 0&1 *)
let median matr =
  let (w,h) = Matrix.get_dims(matr) in
  let newMatr = Matrix.make w h (0) in
  begin
    for y = 0 to (h-1) do
      for x = 0 to (w-1) do
	let i = ref 0 in
	for b = (y-1) to (y+1) do
	  for a = (x-1) to (x+1) do
	    if (a>=0 && a<=w-1 && b>=0 && b<=h-1) then
	      i := !i + matr.(a).(b);
 	  done;
	done;
	if (!i >= 5) then
	  newMatr.(x).(y) <- 1
	else
	  newMatr.(x).(y) <- 0;
      done;
    done;
    newMatr
  end

(* Applique la convolution sur un pixel 
let pixel_convolution kernel matrix x y =
begin
  let size = (Array.length kernel) / 2 in
  let value = ref 0.0 in
  let pixel = ref 0.0 in
  let (w,h) = Matrix.get_dims(matrix) in
  for i = -size to size do
    for j = -size to size do
      begin
      if ((x+i > 0) && (x+i < w)) &&
         ((y+j > 0) && (y+j < h)) then
        pixel := float_of_int(matrix.(x+i).(y+j)*255) (* AJOUT DE *255 POUR LES TESTS !! !! !! !! !! !! !! !! !! !! !! !! *)
      else	
        pixel := 0.0;
      end;
      value := !value +. kernel.(size+i).(size+j) *. !pixel;
    done;
  done;
  if (!value > 255.) then
    begin
    Printf.printf "lol";
      255
    end
  else 
    begin
      print_int (int_of_float(!value));
      int_of_float(!value);
    end
 end

(* Applique la convolution sur chaque pixel 
   à partir du kernel donné *)
let convolution kernel matrix =
  let (w,h) = Matrix.get_dims matrix in
    let new_matrix = Matrix.make w h (0,0,0) in
        for y = 0 to (h-1) do
            for x = 0 to (w-1) do
	      let g = (pixel_convolution kernel matrix x y) in
              new_matrix.(x).(y) <- (g,g,g);
            done;
        done;
    new_matrix;;

(* Filtre gaussien *)
let kernel_gaussian =
    [| [| 1. /. 16.; 2. /. 16.; 1. /. 16. |] ;
       [| 2. /. 16.; 4. /. 16.; 2. /. 16. |] ;
       [| 1. /. 16.; 2. /. 16.; 1. /. 16. |] |]

(* Application d'un filtre gaussien *)
let gaussian_convolution img =
  let matrix = Matrix.img2matr img in
  let new_matrix = convolution kernel_gaussian matrix in
  Matrix.print_matrix matrix;
  Matrix.matr2img2 new_matrix;;
*)

(* Applique la convolution sur un pixel *)
let pixel_convolution kernel img x y =
begin
  let size = (Array.length kernel) / 2 in
  let value = ref 0.0 in
  let pixel = ref 0.0 in
  let (w,h) =  Basic_tools.get_dims(img) in
  for i = (-size) to size do
    for j = (-size) to size do
      if ((x+i > 0) && (x+i < w)) &&
         ((y+j > 0) && (y+j < h)) then
        pixel := float_of_int( Basic_tools.onlyone (Sdlvideo.get_pixel_color img (x+i) (y+i) ) )
      else
	pixel := 0.0;
     (* if ( y = h/2 ) then
	begin
	  print_float (!pixel);
	  Printf.printf " * ";
	end;*)
      value := !value +. (kernel.(size+i).(size+j) *. !pixel);
    done;
  done;
  (*if ( y = h/2 ) then
  begin
    Printf.printf " = ";
    print_float (!value);
    Printf.printf " | ";
  end;*)
  if (!value > 255.) then
      255
  else
    (*begin
      if ( y = (h/2)-1 || y = (h/2)+1 ) then
	120
      else*)
	int_of_float(!value)
    (*end;*)
end

(* Applique la convolution sur chaque pixel 
   à partir du kernel donné *)
let convolution kernel img =
  let (w,h) = Basic_tools.get_dims(img) in
  let new_img = Sdlvideo.create_RGB_surface_format img [] w h in
        for y = 0 to (h-1) do
            for x = 0 to (w-1) do
	      let g = (pixel_convolution kernel img x y) in
              Sdlvideo.put_pixel_color new_img x y (g,g,g);
            done;
        done;
  new_img

(* Filtre gaussien *)
let kernel_gaussian =
    [| [| 1. /. 16.; 2. /. 16.; 1. /. 16. |] ;
       [| 2. /. 16.; 4. /. 16.; 2. /. 16. |] ;
       [| 1. /. 16.; 2. /. 16.; 1. /. 16. |] |];;
