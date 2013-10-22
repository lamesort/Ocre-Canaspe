let nbcols = Array.length
let nbrows x = Array.length x.(0)

let get_dims m = (nbcols m, nbrows m)
let make = Array.make_matrix

(* Surface (255,255,255)/(0,0,0) -> Matrice *)
let img2matr img =
  begin
    let (w,h) = Basic_tools.get_dims(img) in
    let matr = make w h (0) in
    for y = 0 to h-1 do
      for x = 0 to w-1 do
	if Sdlvideo.get_pixel_color img x y = (0,0,0) then
          matr.(x).(y) <- 0
	else
          matr.(x).(y) <- 1
      done;
    done;
      matr
  end

(* Matrice (1/0) -> Surface *)
let matr2img matr =
  begin
    let (w,h) = get_dims(matr) in
    let z32 = Int32.zero in
    let img = Sdlvideo.create_RGB_surface [] w h 16 z32 z32 z32 z32 in
    for y = 0 to h -1 do
      for x = 0 to w-1 do
	if matr.(x).(y) = 0 then
          Sdlvideo.put_pixel_color img x y (0,0,0)
	else
          Sdlvideo.put_pixel_color img x y (255,255,255)
      done;
    done;
    img
  end

(* Surface (255,255,255)/(0,0,0) -> Matrice *)
let img2matr2 img =
  begin
    let (w,h) = Basic_tools.get_dims(img) in
    let matr = make w h (0) in
    for y = 0 to h-1 do
      for x = 0 to w-1 do
	if Sdlvideo.get_pixel_color img x y = (0,0,0) then
          matr.(x).(y) <- 0
	else
          matr.(x).(y) <- 255
      done;
    done;
      matr
  end

(* Matrice (x,x,x) -> Surface *)
let matr2img2 matr =
  begin
    let (w,h) = get_dims(matr) in
    let z32 = Int32.zero in
    let img = Sdlvideo.create_RGB_surface [] w h 16 z32 z32 z32 z32 in
    for y = 0 to h -1 do
      for x = 0 to w-1 do
        Sdlvideo.put_pixel_color img x y (matr.(x).(y));
      done;
    done;
    img
  end



(* Affichage de la matrice en console *)
let print_matrix matr =
  begin
    let (w,h) = get_dims matr in
    for y = 0 to (h-1) do
      for x = 0 to (w-1) do
	print_int matr.(x).(y);
      done;
	Printf.printf "\n";
    done;
  end
