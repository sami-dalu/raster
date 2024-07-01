open Core

(* You need to change the implementation of this function so that it does
   something to the image instead of just leaving it untouched. *)
let transform image =
  Image.map image ~f:(fun (r, g, b) ->
    let avg = (r + g + b) / 3 in
    avg, avg, avg)
;;

let command =
  Command.basic
    ~summary:"Convert an image to grayscale"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_gray.ppm")]
;;

let find_diff img1 img2 : (int * int) list =
  Image.foldi img1 ~init:[] ~f:(fun ~x:x1 ~y:y1 (l : (int * int) list) pix ->
    if Pixel.equal pix (Image.get img2 ~x:x1 ~y:y1) then l else (x1, y1) :: l)
;;

let%expect_test "grayscale backpacker" =
  let grayscaled =
    transform (Image.load_ppm ~filename:"../images/beach_portrait.ppm")
  in
  let original =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_gray.ppm"
  in
  List.iter (find_diff grayscaled original) ~f:(fun (a, b) ->
    printf "%d %d\n" a b);
  print_string "All done!";
  [%expect "All done!"]
;;
