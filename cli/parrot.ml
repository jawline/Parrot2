open Core

let input_dir_param = Command.Param.(anon ("input_directory" %: string))
let output_dir_param = Command.Param.(anon ("output_directory" %: string))

let run_program input_dir output_dir =
  printf "%s %s" input_dir output_dir;
  Parrot_core.clean_build input_dir output_dir;
  ()
;;

let command =
  Command.basic
    ~summary:"Parrot static-website generator CLI"
    ~readme:(fun () -> "README.md for detailed documentation")
    Command.Param.(
      map (both input_dir_param output_dir_param) ~f:(fun (ind, outd) ->
        (fun () -> run_program ind outd)
      )
    )
;;

let () =
  Command.run ~version:"1.0" ~build_info:"TRIAL" command
