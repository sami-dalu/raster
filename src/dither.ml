open Core

(* This should look familiar by now! *)
let distribute_error ~x ~y ~err ~img =
  if x + 1 < Image.width img
  then (
    let r = Float.of_int (Pixel.red (Image.get img ~x:(x + 1) ~y)) in
    let g = Float.of_int (Pixel.green (Image.get img ~x:(x + 1) ~y)) in
    let b = Float.of_int (Pixel.blue (Image.get img ~x:(x + 1) ~y)) in
    let right_pxl =
      ( Int.of_float (r +. (7. /. 16. *. err))
      , Int.of_float (g +. (7. /. 16. *. err))
      , Int.of_float (b +. (7. /. 16. *. err)) )
    in
    Image.set img ~x:(x + 1) ~y right_pxl);
  if y + 1 < Image.height img
  then (
    let r = Float.of_int (Pixel.red (Image.get img ~x ~y:(y + 1))) in
    let g = Float.of_int (Pixel.green (Image.get img ~x ~y:(y + 1))) in
    let b = Float.of_int (Pixel.blue (Image.get img ~x ~y:(y + 1))) in
    let below_pxl =
      ( Int.of_float (r +. (5. /. 16. *. err))
      , Int.of_float (g +. (5. /. 16. *. err))
      , Int.of_float (b +. (5. /. 16. *. err)) )
    in
    Image.set img ~x ~y:(y + 1) below_pxl);
  if y + 1 < Image.height img && x - 1 >= 0
  then (
    let r = Float.of_int (Pixel.red (Image.get img ~x:(x - 1) ~y:(y + 1))) in
    let g =
      Float.of_int (Pixel.green (Image.get img ~x:(x - 1) ~y:(y + 1)))
    in
    let b =
      Float.of_int (Pixel.blue (Image.get img ~x:(x - 1) ~y:(y + 1)))
    in
    let bl_pxl =
      ( Int.of_float (r +. (3. /. 16. *. err))
      , Int.of_float (g +. (3. /. 16. *. err))
      , Int.of_float (b +. (3. /. 16. *. err)) )
    in
    Image.set img ~x:(x - 1) ~y:(y + 1) bl_pxl);
  if y + 1 < Image.height img && x + 1 < Image.width img
  then (
    let r = Float.of_int (Pixel.red (Image.get img ~x:(x + 1) ~y:(y + 1))) in
    let g =
      Float.of_int (Pixel.green (Image.get img ~x:(x + 1) ~y:(y + 1)))
    in
    let b =
      Float.of_int (Pixel.blue (Image.get img ~x:(x + 1) ~y:(y + 1)))
    in
    let br_pxl =
      ( Int.of_float (r +. (1. /. 16. *. err))
      , Int.of_float (g +. (1. /. 16. *. err))
      , Int.of_float (b +. (1. /. 16. *. err)) )
    in
    Image.set img ~x:(x + 1) ~y:(y + 1) br_pxl)
;;

let _transform image =
  let grayscale = Grayscale.transform image in
  let max_val = Image.max_val grayscale in
  let _ =
    Image.mapi grayscale ~f:(fun ~x:x1 ~y:y1 (_, _, _) ->
      let red = Pixel.red (Image.get grayscale ~x:x1 ~y:y1) in
      if Float.( >. ) (Float.of_int red) (Float.of_int max_val *. 0.5)
      then (
        Image.set grayscale ~x:x1 ~y:y1 (max_val, max_val, max_val);
        distribute_error
          ~x:x1
          ~y:y1
          ~err:(Float.of_int (red - max_val))
          ~img:grayscale;
        Image.get grayscale ~x:x1 ~y:y1)
      else (
        Image.set grayscale ~x:x1 ~y:y1 (0, 0, 0);
        distribute_error ~x:x1 ~y:y1 ~err:(Float.of_int red) ~img:grayscale;
        Image.get grayscale ~x:x1 ~y:y1))
  in
  grayscale
;;

let populate_palette ~max ~n =
  let palette = Array.create ~len:n 0 in
  Array.mapi palette ~f:(fun index _ ->
    if index = 0 then 0 else index * max / (n - 1))
;;

let closest_color palette ~c ~max ~n =
  let color = if c < 0 then 0 else c in
  let interval_size = max / (n - 1) in
  let index =
    Float.round_nearest (Float.of_int color /. Float.of_int interval_size)
  in
  (* if Int.of_float index >= n then *)
  printf "out of bounds %d %d %f\n" color interval_size index;
  (* printf "color: %d interval_size:%d\n" c interval_size; *)
  palette.(Int.of_float index)
;;

let distribute_error_color ~x ~y ~err ~img ~c =
  let new_pixel ~x1 ~y1 =
    let r, g, b = Image.get img ~x:x1 ~y:y1 in
    let new_pixel =
      match c with
      | 'r' -> r + err, g, b
      | 'g' -> r, g + err, b
      | _ -> r, g, b + err
    in
    Image.set img ~x:x1 ~y:y1 new_pixel
  in
  if x + 1 < Image.width img then new_pixel ~x1:(x + 1) ~y1:y;
  if y + 1 < Image.height img then new_pixel ~x1:x ~y1:(y + 1);
  if y + 1 < Image.height img && x - 1 >= 0
  then new_pixel ~x1:(x - 1) ~y1:(y + 1);
  if y + 1 < Image.height img && x + 1 < Image.width img
  then new_pixel ~x1:(x + 1) ~y1:(y + 1)
;;

let color_transform image n =
  let palette = populate_palette ~max:(Image.max_val image) ~n in
  let image_max = Image.max_val image in
  let _ =
    Image.mapi image ~f:(fun ~x:x1 ~y:y1 (r, g, b) ->
      printf "x = %d y = %d %d %d %d\n" x1 y1 r g b;
      let new_r = closest_color palette ~c:r ~n ~max:image_max in
      let new_g = closest_color palette ~c:g ~n ~max:image_max in
      let new_b = closest_color palette ~c:b ~n ~max:image_max in
      Image.set image ~x:x1 ~y:y1 (new_r, new_g, new_b);
      distribute_error_color ~x:x1 ~y:y1 ~err:(new_r - r) ~c:'r' ~img:image;
      distribute_error_color ~x:x1 ~y:y1 ~err:(new_g - g) ~c:'g' ~img:image;
      distribute_error_color ~x:x1 ~y:y1 ~err:(new_b - b) ~c:'b' ~img:image;
      Image.get image ~x:x1 ~y:y1)
  in
  image
;;

(* let _ = Image.mapi grayscale ~f:(fun ~x:x1 ~y:y1 (_, _, _) -> let red =
   Pixel.red (Image.get grayscale ~x:x1 ~y:y1) in if Float.( >. )
   (Float.of_int red) (Float.of_int max_val *. 0.5) then ( Image.set
   grayscale ~x:x1 ~y:y1 (max_val, max_val, max_val); distribute_error ~x:x1
   ~y:y1 ~err:(Float.of_int (red - max_val)) ~img:grayscale; Image.get
   grayscale ~x:x1 ~y:y1) else ( Image.set grayscale ~x:x1 ~y:y1 (0, 0, 0);
   distribute_error ~x:x1 ~y:y1 ~err:(Float.of_int red) ~img:grayscale;
   Image.get grayscale ~x:x1 ~y:y1)) in grayscale *)

let command =
  Command.basic
    ~summary:"Dither an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and num =
        flag
          "num"
          (required Command.Param.int)
          ~doc:"num, number of colors per channel"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let transformed_image = color_transform image num in
        Image.save_ppm
          transformed_image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm"
             ^ "_dither_color.ppm")]
;;
