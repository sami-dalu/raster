open Core



(* let energy ~x ~y ~r ~g ~b = 

;; *)
(* You need to change the implementation of this function so that it replaces
   the "blue" pixels of the foreground image with pixels from the
   corresponding position in the background image instead of just ignoring
   the background image and returning the foreground image. *)
let get_neigbhors ~x ~y img = 
  let possible_neighbors = [if (x+1) < Image.width img then Some (Image.get img ~x:(x+1) ~y:y) else None; 
  if (y+1) < Image.height img then Some (Image.get img ~x:x ~y:(y+1)) else None;
  if (x-1) >=0 then Some (Image.get img ~x:(x-1) ~y:(y)) else None;
  if (y-1) >=0 then Some (Image.get img ~x:(x) ~y:(y-1)) else None;
  if (x-1) >=0 && (y-1) >=0 then Some (Image.get img ~x:(x-1) ~y:(y-1)) else None;
  if (x+1) < Image.width img && (y-1) >=0 then Some (Image.get img ~x:(x+1) ~y:(y-1)) else None;
  if (x-1) >= 0 && (y+1) < Image.height img then Some (Image.get img ~x:(x-1) ~y:(y+1)) else None;
  if (x+1) < Image.width img && (y+1) < Image.height img then Some (Image.get img ~x:(x+1) ~y:(y+1)) else None
  ] in
  List.filter_opt possible_neighbors
let transform ~foreground ~background =
  let is_blue r g b =
    10 * b * b >  19 * (r*r + g*g)
  in
  Image.mapi foreground ~f:(fun ~x:x1 ~y:y1 (r1, g1, b1) ->
    (* is_blue r1 g1 b1 &&  *)
    if is_blue r1 g1 b1 && List.count (get_neigbhors ~x:x1 ~y:y1 foreground) ~f:(fun (r, g, b) -> is_blue r g b) >= 3 then (
      Image.get background ~x:x1 ~y:y1)
   else r1, g1, b1
  )
;;
let command =
  Command.basic
    ~summary:
      "Replace the 'blue' pixels of an image with those from another image"
    [%map_open.Command
      let foreground_file =
        flag
          "foreground"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the foreground PPM image file"
      and background_file =
        flag
          "background"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the background PPM image file"
      in
      fun () ->
        let foreground = Image.load_ppm ~filename:foreground_file in
        let background = Image.load_ppm ~filename:background_file in
        let image' = transform ~foreground ~background in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn foreground_file ~suffix:".ppm"
             ^ "_vfx.ppm")]
;;

let find_diff img1 img2 : (int * int) list =
  Image.foldi img1 ~init:[] ~f:(fun ~x:x1 ~y:y1 (l : (int * int) list) pix ->
    if Pixel.equal pix (Image.get img2 ~x:x1 ~y:y1) then l else (x1, y1) :: l)
;;

let%expect_test "blue oz" =
  let blued =
    transform
      ~foreground:(Image.load_ppm ~filename:"../images/oz_bluescreen.ppm")
      ~background:(Image.load_ppm ~filename:"../images/meadow.ppm")
  in
  let reference =
    Image.load_ppm ~filename:"../images/reference-oz_bluescreen_vfx.ppm"
  in
  List.iter (find_diff blued reference) ~f:(fun (a, b) ->
    printf "%d %d\n" a b);
  print_string "All done!";
  [%expect "All done!"]
;;
