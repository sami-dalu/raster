open Core

(* You need to modify this function to blur the input image based on the
   provided radius instead of ignoring it. *)
let transform image ~radius =
  Image.mapi image ~f:(fun ~x:x1 ~y:y1 _ ->
    let lower_x = if x1 - radius < 0 then 0 else x1 - radius in
    let higher_x =
      if x1 + radius > Image.width image - 1
      then Image.width image - 1
      else x1 + radius
    in
    let lower_y = if y1 - radius < 0 then 0 else y1 - radius in
    let higher_y =
      if y1 + radius > Image.height image - 1
      then Image.height image - 1
      else y1 + radius
    in
    let r_square =
      Image.slice
        image
        ~x_start:lower_x
        ~x_end:higher_x
        ~y_end:higher_y
        ~y_start:lower_y
    in
    Image.mean_pixel r_square)
;;

(* ask TA, move on, why do i need -1? *)

let command =
  Command.basic
    ~summary:"Blur an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and radius =
        flag
          "radius"
          (required Command.Param.int)
          ~doc:"N the radius to use when blurring (higher = more blurred)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~radius in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_blur.ppm")]
;;

let find_diff img1 img2 : (int * int) list =
  Image.foldi img1 ~init:[] ~f:(fun ~x:x1 ~y:y1 (l : (int * int) list) pix ->
    if Pixel.equal pix (Image.get img2 ~x:x1 ~y:y1) then l else (x1, y1) :: l)
;;

let%expect_test "blue oz" =
  let blurred =
    transform
      (Image.load_ppm ~filename:"../images/beach_portrait.ppm")
      ~radius:3
  in
  let reference =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_blur.ppm"
  in
  List.iter (find_diff blurred reference) ~f:(fun (a, b) ->
    printf "%d %d\n" a b);
  print_string "All done!";
  [%expect "All done!"]
;;
