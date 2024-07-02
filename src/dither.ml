open Core

(* This should look familiar by now! *)
let transform image =
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
      let r =
        Float.of_int (Pixel.red (Image.get img ~x:(x - 1) ~y:(y + 1)))
      in
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
      let r =
        Float.of_int (Pixel.red (Image.get img ~x:(x + 1) ~y:(y + 1)))
      in
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
  in
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

let command =
  Command.basic
    ~summary:"Dither an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
