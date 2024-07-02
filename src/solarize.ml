open Core

let invert_color ~c ~max = if 10 * c >= 4 * max then max - c else c

let transform image =
  Image.map image ~f:(fun (r, g, b) ->
    let max_color = Image.max_val image in
    ( invert_color ~c:r ~max:max_color
    , invert_color ~c:g ~max:max_color
    , invert_color ~c:b ~max:max_color ))
;;

let command =
  Command.basic
    ~summary:"Solarize an image"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_solarize.ppm")]
;;
