open Core

let is_blue r g b = b > r + g

(* You need to change the implementation of this function so that it replaces
   the "blue" pixels of the foreground image with pixels from the
   corresponding position in the background image instead of just ignoring
   the background image and returning the foreground image. *)
let transform ~foreground ~background =
  Image.mapi foreground ~f:(fun ~x:x1 ~y:y1 (r1, g1, b1) ->
    if is_blue r1 g1 b1 then Image.get background ~x:x1 ~y:y1 else r1, g1, b1)
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
