open Core

exception InvalidArticleName of string

let dedup str =
  let rec dedup_list = function
    | [] -> []
    | '_' :: '_' :: xs -> dedup_list ('_' :: xs)
    | x :: xs -> x :: dedup_list xs
  in
  String.of_char_list (dedup_list (String.to_list str))
;;

let sanitize_path = function
  | "" -> raise (InvalidArticleName "all whitespace")
  | name ->
    name
    |> String.substr_replace_all ~pattern:" " ~with_:"_"
    |> String.substr_replace_all ~pattern:"-" ~with_:"_"
    |> String.substr_replace_all ~pattern:"," ~with_:"_"
    |> String.substr_replace_all ~pattern:"'" ~with_:""
    |> String.substr_replace_all ~pattern:"\"" ~with_:""
    |> String.substr_replace_all ~pattern:":" ~with_:""
    |> String.substr_replace_all ~pattern:"." ~with_:"_"
    |> String.substr_replace_all ~pattern:"&" ~with_:"and"
    |> dedup
;;

(* TODO: Images will eventually get a dependency tree and automatic web optimization as they do in Parrot1, but for now just copy them into the result build in the correct place *)
let image_path path = sprintf "/images/%s" path
