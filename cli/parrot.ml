open Core

let input_dir_param = Command.Param.(anon ("input_directory" %: string))

let command =
  Command.basic
    ~summary:"Parrot static-website generator CLI"
    ~readme:(fun () -> "README.md for detailed documentation")
    Command.Param.(
      map input_dir_param ~f:(fun input_dir () -> Parrot_core.clean_build input_dir))
;;

let () = Command_unix.run ~version:"1.0" ~build_info:"TRIAL" command
